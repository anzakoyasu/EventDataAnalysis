import processing.core.PApplet
import processing.core.PVector
import java.util.ArrayList

//EventGroupを投げるとそれをChronoViewに近づけた座標を返す

class Node(sx:Float, sy: Float, val gx:Float, val gy: Float, val r:Float){
  var tx = sx
  var ty = sy
  var dx = 0f
  var dy = 0f
}

class GraphEades(val repulsion:Float, val gravity:Float, val gravity_clock:Float, edge_num:Int, node_num:Int, eglist:Iterable[EventGroup]) extends Graph(edge_num,node_num,eglist){


  def eades(i: Int, n1: Node):(Float,Float) = {
    var powerx = 0f;
    var powery = 0f;

    for (j <- 0 to node_num-1) {
      if (i != j){
        val n2 = tmpeglist(j)
        val d = PApplet.dist(n1.tx, n1.ty, n2.tx, n2.ty) + 0.000001;

        if (aam(i)(j) != 1 || desired_dist(i)(j) >= 5) {
          val distX = n2.tx - n1.tx;
          val distY = n2.ty - n1.ty;
          val rsq = distX * distX + distY * distY + 0.00001;

          val cosTheta = distX / d;
          val sinTheta = distY / d;

          powerx = powerx + ((-repulsion / rsq ) * cosTheta).asInstanceOf[Float]
          powery = powery + ((-repulsion / rsq ) * sinTheta).asInstanceOf[Float]
        }
      }
    }

    for (j <- 0 to node_num-1) {
      if (i != j && aam(i)(j) != 0) {
        val n2 = tmpeglist(j);
        val distX = n2.tx - n1.tx;
        val distY = n2.ty - n1.ty;
        val d = PApplet.dist(n1.tx, n1.ty, n2.tx, n2.ty) + 0.000001;

        if (desired_dist(i)(j) >= 5 || d >= 50){
          val cosTheta = distX / d;
          val sinTheta = distY / d;
          val logdwd0 = Math.log(d / (desired_dist(i)(j)+ 0.000001) );
          powerx = powerx + (gravity * logdwd0 * cosTheta).asInstanceOf[Float]
          powery = powery + (gravity * logdwd0 * sinTheta).asInstanceOf[Float]
        }
      }
    }
    return (powerx,powery)
  }

  def closeChrono(i: Int, n1:Node):(Float, Float) ={
    var b = PApplet.dist(n1.tx, n1.ty, n1.gx, n1.gy) / (n1.r*0.7);
    b=1;
    //if (b < 1.0) b = 1.0;
    //if (b > 3.0) b = 3.0;
    val powerx = (n1.gx - n1.tx) * gravity_clock * b.asInstanceOf[Float]
    val powery = (n1.gy - n1.ty) * gravity_clock * b.asInstanceOf[Float]
    return (powerx, powery);
  }

  def draw(): Array[Position] = {
    initialize()
    //収束するまで繰り返し
    var c = 0
    while(c < 1000){
      for (i <- 0 to node_num-1) {//Test_Node→hashmap
        val n1 = tmpeglist(i);

        //0.8 = 減衰率
        val power = eades(i, n1);
        n1.dx = (n1.dx + power._1)*(0.8f)
        n1.dy = (n1.dy + power._2)*(0.8f)
        n1.tx = n1.tx + n1.dx;
        n1.ty = n1.ty + n1.dy;
        n1.dx = 0
        n1.dy = 0

        val powerc = closeChrono(i, n1);
        n1.dx = (n1.dx + powerc._1)*(0.8f)
        n1.dy = (n1.dy + powerc._2)*(0.8f)
        n1.tx = n1.tx + n1.dx;
        n1.ty = n1.ty + n1.dy;

        if (n1.tx < 50-350) n1.tx = 50-350;
        if (n1.ty < 50-350) n1.ty = 50-350;
        if (n1.tx > 650-350) n1.tx = 650-350;
        if (n1.ty > 650-350) n1.ty = 650-350;
      }

      c = c + 1
    }
    val pos = new Array[Position](node_num)
    tmpeglist.zipWithIndex.foreach{ eg =>
      pos(eg._2) = new Position(eg._1.tx, eg._1.ty)
    }
    return pos
  }

}

case class Point(p: PVector, h:Int, c: Int){
    var vec = new PVector(0,0)
    var ei = -1;
}


class Graph(val edge_num :Int, val node_num :Int, val eglist :Iterable[EventGroup]) {
  val desired_dist = Array.ofDim[Float](node_num ,node_num)
  val aam = Array.ofDim[Int](node_num,node_num )
  var fcount = 0

  val tmpeglist = new Array[Node](node_num)
  eglist.zipWithIndex.foreach{ ei =>
    val eg = ei._1
    val i = ei._2
    val p = eg.chronoPoint()
    val mdspos = MDSPositions.get(i)
    tmpeglist(i) = new Node(mdspos.x.asInstanceOf[Float],mdspos.y.asInstanceOf[Float],p.x,p.y,eg.getDiameter())
  }

  def initialize() {
    //makeNodeLink();
    createEdgeAll()
    setDesiredDistance()
  }

  def reset() {
    initializeCount();
  }

  def initializeCount() {
    fcount = 0;
  }

  def addCount() {
    fcount = fcount + 1
  }

  def getCount() {
    return fcount
  }

  def ccc() : ArrayList[Point] = {
    val points = new ArrayList[Point]();
    var dh = 0.2f;

    for (c <- 0 to 8) {
      val r = c * ViewSet.r_view / 9;
      dh = 0.5f / (c+1);
      var h = 0f
      while (h < EventData.period) {
        val theta = ViewSet.getTheta(h, EventData.period);
        val x = r * Math.cos(theta);
        val y = r * Math.sin(theta);
        points.add(new Point(new PVector(x.asInstanceOf[Float],y.asInstanceOf[Float]*(-1)),h.asInstanceOf[Int],c));
        h = h + dh
      }
    }

    for(i<- 0 to points.size()-1 ){
      val point = points.get(i);
      val p = new PVector(0,0);
      for(j <- 0 to tmpeglist.size-1){
        val node = tmpeglist(j);
        var f = PApplet.dist(node.gx,node.gy,point.p.x,point.p.y);
        var d = PApplet.dist(node.tx,node.ty,node.gx,node.gy);
        d = PApplet.exp(d*0.001f);
        f = PApplet.exp(f*0.08f);
        p.x = p.x +(node.tx - point.p.x)/f*d;
        p.y = p.x +(node.ty - point.p.y)/f*d;
      }
      point.vec = p;
    }

    for(n <- 0 to 4){
      for(i <- 0 to points.size()-1){
        val pi = points.get(i)
        //print(pi.vec);
        for(j<-0 to  points.size()-1){
          if(i!=j){
            val pj = points.get(j)
            val d = 1.0-PApplet.map(PVector.dist(pi.p,pj.p),0,600,0.0f,1.0f);

            if(calcAngle(pi.vec,pj.vec) >= 0.3) {
              if(pj.vec.mag() >= 5 || pi.vec.mag() >= 20){
                val vi = new PVector(pi.vec.x,pj.vec.y);
                pj.vec.mult(0.99999f).add(vi.mult(0.00001f));
              }
            }
          }
        }
      }
    }

    return points
  }


  def calcAngle(v1:PVector , v2:PVector ): Float={
    val costheta = v1.dot(v2) / (v1.mag()*v2.mag());
    return Math.abs(costheta);
  }

/*
  def makeNodeLink() {
    val node_border = 600f;
    initializeCount();

    for (i <- 0 to node_num-1) {//Test_Node→hashmap
      val n1 = eglist(i)
      var s = 0
      val i_stack = Array[Int](edge_num)
      val d_stack = Array[Float](edge_num)
      d_stack(s) = 10000;

      for (j <- 0 to node_num-1) {
        //if (i == j) continue;
        val n2 = eglist(j);
        val d = PApplet.dist(n1.x, n1.y, n2.x, n2.y);
        if (d_stack(s) > d) {
          if (s < d_stack.length-1) {
            d_stack(s+1) = d_stack(s);
            i_stack(s+1) = i_stack(s);
          }
          d_stack(s) = d;
          i_stack(s) = j;

          var k = 0
          for (k <- s; k > 0; k--) {
            if (d < d_stack[k-1]) {
              d_stack[k] = d_stack[k-1];
              i_stack[k] = i_stack[k-1];
            } else {
              break;
            }
          }
          d_stack[k] = d;
          i_stack[k] = j;
          if (s < d_stack.length - 1) s++;
        }
      }
      for (h <- 0 to edge_num-1) {
        if (d_stack(h) <= node_border) aam(i)(i_stack(h)) = 1;
      }
    }
  }*/

  def createEdgeAll() {
    for (i <- 0 to node_num -2) for (j <- i+1 to node_num - 1) aam(i)(j) = 1;
  }

  def setDesiredDistance() {
    eglist.zipWithIndex.foreach { ei =>
      val n1 = ei._1
      val m1p = MDSPositions.get(ei._2)
      eglist.zipWithIndex.foreach { ej =>
        val n2 = ej._1
        val m2p = MDSPositions.get(ej._2)
        val dist = PApplet.dist(m1p.x.asInstanceOf[Float],m1p.y.asInstanceOf[Float],m2p.x.asInstanceOf[Float],m2p.y.asInstanceOf[Float])
        desired_dist(ei._2)(ej._2) = dist
        desired_dist(ej._2)(ei._2) = dist
      }
    }
  }

/*  def drawEdge() {
    for (i <- 0 to node_num-2) {
      for (j <-i+1 to node_num-1) {
        if (aam(i)(j) != 0) {
          stroke(255, 128);
          float w =0.5;
          strokeWeight(w);
          line(eglist(i).x, eglist(i).y, eglist(j).x, eglist(j).y);
        }
      }
    }
  }*/
  /*
  void bbb(){
    float dh = 0.2;
    ArrayList circles_c = new ArrayList();

    for (int c = 0; c < 10; c++) {
      float r = c * 300 / 10;
      dh = map(r,0,300,0.5,0.01);
      dh = Math.round(dh*100)/100;
      while(period % dh != 0){
        dh+=0.1;
      }
      dh = 0.0625*2;
      ArrayList<Node> point = new ArrayList<Node>();
      for (float h = 0; h < period; h += dh) {
        float theta = getTheta(h, period);
        float x = r * cos(theta);
        float y = r * sin(theta);
        point.add(new Node(x,y,h));
      }
      circles_c.add(point);
    }

    for(int k = 0; k < circles_c.size(); k++){
      ArrayList<Node> aaa = (ArrayList<Node>)circles_c.get(k);
      for(int i=0; i < aaa.size(); i++){
        Node point = aaa.get(i);
        PVector p = new PVector();
        for(int j=0; j < test_Node.size(); j++){
          Node node = test_Node.get(j);
          float f = dist(node.gx,node.gy,point.x,point.y);
          if(f > 100) continue;
          float d = dist(node.x,node.y,node.gx,node.gy);
          //f*=0.09; d*=0.003;
          d = exp(d*0.001);

          f = exp(f*0.06);
          p.x += (node.x - point.x) /f*d;
          p.y += (node.y - point.y) /f*d;
        }
        point.x += p.x;
        point.y += p.y;
        stroke(gcl_h[(int)point.h]);
        stroke(lerpColor(color(gcl_h[(int)point.h]), color(255, 255, 255, 255),map(mag(point.x,point.y),0,300,0.7,0)));

        if(i>0){
          strokeWeight(map(dist(aaa.get(i-1).x,aaa.get(i-1).y,point.x,point.y),0,100,3,8));
          line(aaa.get(i-1).x,aaa.get(i-1).y*-1,point.x,point.y*-1);
        }
        strokeWeight(3);
      }
      Node first = aaa.get(0);
      stroke(gcl_h[(int)first.h]);
      stroke(lerpColor(color(gcl_h[(int)first.h]), color(255, 255, 255, 255),map(mag(first.x,first.y),0,300,0.7,0)));
      line(aaa.get(aaa.size()-1).x,aaa.get(aaa.size()-1).y*-1,first.x,first.y*-1);
    }
  }*/


  /*def drawDiff() {
    strokeWeight(3);
    bbb();
    strokeWeight(1);
  }*/

}