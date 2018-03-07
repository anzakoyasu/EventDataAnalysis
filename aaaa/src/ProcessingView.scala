import scala.collection.JavaConversions._
import scala.collection.mutable
import java.awt._
import javax.swing._
import java.awt.Dimension
import java.awt.event._
import java.util.ArrayList
import java.util.Hashtable
import java.awt.Graphics2D
import processing.core.PApplet
import processing.core.PSurface
import processing.core.PConstants

import java.text.SimpleDateFormat
import java.util.Date

object ViewSet{
  val r_view =230

  val gcl_h = Array(16331852,16468280,16606765,16744740,16751381,16758301,16765222,16772399,
                    14280239,11392812,8964154,7060043,4697177,3122285,2269830,209998,
                    2199205,2523310,3567792,6186670,8675756,10902185,12735647,14437781)

  def getTheta(t: Float, period: Float): Float = {
    return PConstants.PI/2 - 2*PConstants.PI*(t/period);
  }

  var selectedGroup :String = ""
  var display_glyph = true
  var display_label = true
  var isSelectLocking = false

  var points:ArrayList[Point] = null
}

class DTWViewApplet(f: JFrame) extends PApplet {
  setSize(370,580);
  try {
    val handleSettingsMethod: java.lang.reflect.Method =
        this.getClass().getSuperclass().getDeclaredMethod("handleSettings", null);
    handleSettingsMethod.setAccessible(true);
    handleSettingsMethod.invoke(this, null);
  } catch {
    case e:Throwable => e.printStackTrace()
  }

  val surfacehh = super.initSurface()
  val canvas= surfacehh.getNative().asInstanceOf[Canvas];
  surfacehh.placeWindow(Array(0, 0), Array(0, 0));
  val panel = new JPanel()
  panel.add(canvas)
  f.add(panel,BorderLayout.CENTER);
  //this.showSurface()
  this.startSurface();

  override def settings() {
    size(370, 580)
  }
  override def setup(){
    background(255, 255, 255);

  }

  var beforeeg = ""

  override def draw(){
    if(beforeeg != ViewSet.selectedGroup){
      colorMode(PConstants.RGB)
      background(255,255,255)
      EventData.eventGroups() match {
        case Some(eg) => drawEventGroups(eg)
        case None => return
      }
    }

    beforeeg = ViewSet.selectedGroup
  }

  def drawEventGroups(eventGroupsMap: mutable.Map[String,EventGroup]) = {
    val eventGroups   = eventGroupsMap.values

    if(eventGroupsMap.get(ViewSet.selectedGroup) != None){
      val eg:EventGroup = eventGroupsMap(ViewSet.selectedGroup)
      textSize(10)
      translate(15,100)
      stroke(0)
      line(-50,15,400,15)
      noStroke()

      drawGraph(eg)
      translate(280,-50)
      drawCircleGlyph(eg)
      fill(0,0,0)

      text(ViewSet.selectedGroup,-15f,0f)
      translate(-280,165)

      val dist = EventData.DTWdist(ViewSet.selectedGroup)
      val diststr = eventGroupsMap.keys.zipWithIndex.map{ si=>
        (si._1,dist(si._2))
      }.toArray[(String,Float)]

      val sorted = diststr.sortWith(_._2 < _._2)
      sorted.zipWithIndex.foreach{ sfi=>
        if(sfi._2 < 4 && sfi._1._2 < 100){
          val eg:EventGroup = eventGroupsMap(sfi._1._1)
          drawGraph(eg)
          translate(280,-50)
          drawCircleGlyph(eg)
          fill(0,0,0)
          text(sfi._1._1,-15f,0f)
          translate(-280,150)
        }
      }
    }
  }

 def drawGraph(eg: EventGroup){
      val eventsListByDay = eg.aggressionEventListByDay()
      val w = (250.0 / eventsListByDay.getAllDateCount().asInstanceOf[Float]).asInstanceOf[Float];
      val dateformat = DateFormat.getDateFormat(eventsListByDay.get(0).time)
      val sfd = new SimpleDateFormat("yyyy-MM-dd").format(eventsListByDay.firstDate(dateformat))
      val sld = new SimpleDateFormat("yyyy-MM-dd").format(eventsListByDay.lastDate( dateformat))

      stroke(0, 128);
      line(0, 5, 250, 5);
      fill(0);
      text(sfd, 0  , 10);
      text(sld, 250, 10);
      drawStackedPanel(eg,eventsListByDay,w)
 }


 def drawCumulativeGraph(eg: EventGroup, eventsListByDay: EventsListByDay, w: Float){
      val sx = eventsListByDay.startDateArr
      val th = eventsListByDay.cumHeightArr

      (1 to eventsListByDay.size-1).foreach{ index =>
        val beforeHeight = PApplet.map(th(index-1), 0, eg.eventList.size()+5, 0, 60)
        strokeWeight(1.0f)
        stroke(0,64)
        line(sx(index-1).asInstanceOf[Int]*w, beforeHeight*(-1f), (sx(index).asInstanceOf[Int]-1f)*(w),beforeHeight*(-1f))

        strokeWeight(2.0f);
        val totalHeight = PApplet.map(th(index), 0, eg.eventList.size()+5, 0, 60)
        line((sx(index).asInstanceOf[Int]-1)*w, beforeHeight*(-1f), sx(index).asInstanceOf[Int]*w,totalHeight*(-1f))
      }
      strokeWeight(1.0f);
  }

  def drawStackedPanel(eg: EventGroup,eventsListByDay: EventsListByDay, w:Float){
      val sx = eventsListByDay.startDateArr
      val freq  = eventsListByDay.allDateFreq

      noStroke()
      colorMode(PConstants.HSB,24,100,100)
      (0 to EventData.period-1).foreach{ i =>
        (1 to eventsListByDay.size-1).foreach{ j =>
          val yoko = (1 to eventsListByDay.size-1).map{ k =>
            freq(k)(i)
          }
          val alpha = 255 * PApplet.map(freq(j)(i),0,freq(j).max,0.0f,1.0f) * PApplet.map(freq(j)(i),0,yoko.max,0.0f,1.0f)
          if(alpha > 64){
            fill(color(i,100,100), alpha);
            rect(sx(j).asInstanceOf[Int]*w,-i*4,w*1.5f,4)
          }
        }
      }
      colorMode(PConstants.RGB,255,255,255)
  }

  def drawCircleGlyph(eg: EventGroup) {
      val r_max = eg.getDiameter();
      for (i <- 0 to EventData.period - 1) {
        val nr = r_max /2 * 1.5;
        val dx = nr * Math.cos(ViewSet.getTheta(i, EventData.period));
        val dy = nr * Math.sin(ViewSet.getTheta(i, EventData.period)) * -1;

        val count = eg.freqCount
        val m_i = eg.m_i
        if(count.size < 2) {
          return
        }

        val al = PApplet.map(count(i), 0, count(m_i), 0.4f, 0.8f);
        colorMode(PConstants.HSB,24,100,100)
        fill(color(i,100,100), 128);

        val mr = Math.sqrt(count(m_i) / PConstants.PI);
        val ar = Math.sqrt(count(i) / PConstants.PI) * ((0.5 * r_max/2) / mr);
        ellipse(dx.asInstanceOf[Float], dy.asInstanceOf[Float], ar.asInstanceOf[Float], ar.asInstanceOf[Float]);
        colorMode(PConstants.RGB,255,255,255)
      }
  }

}

class Applet(f: JFrame) extends PApplet {
  setSize(1000, 700)
  smooth(8);
  try {
    val handleSettingsMethod: java.lang.reflect.Method =
        this.getClass().getSuperclass().getDeclaredMethod("handleSettings", null);
    handleSettingsMethod.setAccessible(true);
    handleSettingsMethod.invoke(this, null);
  } catch {
    case e:Throwable => e.printStackTrace()
  }

  val surfacehh = super.initSurface()
  val canvas= surfacehh.getNative().asInstanceOf[Canvas];
  surfacehh.placeWindow(Array(0, 0), Array(0, 0));
  val panel = new JPanel()
  panel.add(canvas)
  f.add(panel,BorderLayout.CENTER);
  //this.showSurface()
  this.startSurface();

  override def settings() {
    size(1000, 700)
    smooth(8);
  }
  override def setup(){
    background(255, 255, 255);

  }

  implicit object DistOrdering extends Ordering[EventGroup]{
    def compare(x: EventGroup, y:EventGroup):Int = {
      if(PApplet.dist(x.pos.x,x.pos.y,mouseX-ViewSet.r_view-50,mouseY-ViewSet.r_view-50)
          > PApplet.dist(y.pos.x,y.pos.y,mouseX-ViewSet.r_view-50,mouseY-ViewSet.r_view-50) ){
        1
      }else{
        -1
      }
    }
  }

  override def mousePressed(){
    ViewSet.isSelectLocking = !ViewSet.isSelectLocking
  }

  var display_edge = true
  override def keyPressed(){

    if(key == 'e'){
      display_edge = !display_edge;
    }
  }

  def drawVecs(){
    if(ViewSet.points == null) return;
    val bp = ViewSet.points.get(0);
    val fp = ViewSet.points.get(0);
    for(i <- 1 to ViewSet.points.size()-1){
      val p = ViewSet.points.get(i);
      strokeWeight(1.0f);

      colorMode(PConstants.HSB,24,100,100)
      stroke(color(p.h,100,100), 128);
      point(p.p.x,p.p.y);
      stroke(color(p.h,100,100), PApplet.map(p.vec.mag(),0,150,50,128));
      line(p.p.x,p.p.y, p.p.x+p.vec.x,p.p.y+p.vec.y);
      colorMode(PConstants.RGB,255,255,255)

    }
    noStroke()
  }


  def drawEventGroups(eventGroupsMap: mutable.Map[String,EventGroup]) = {
    val eventGroups   = eventGroupsMap.values

    if(mouseX < 500 && mouseY < 500 && !ViewSet.isSelectLocking){
      ViewSet.selectedGroup = eventGroups.min.action
    }

    if(display_edge) drawVecs()

    fill(color(200),64)
    eventGroups.foreach{ eg =>
      val r = eg.getDiameter()
      if(eg.action != ViewSet.selectedGroup){
        ellipse(eg.pos.x,eg.pos.y,r,r)
      }
    }

    eventGroups.foreach{ eg =>
      if(eg.action != ViewSet.selectedGroup && ViewSet.display_glyph){
        GlyphDrawer.draw(eg)
      }
    }

    fill(0)
    eventGroups.foreach{ eg =>
      if(eg.action != ViewSet.selectedGroup && ViewSet.display_label){
        textSize(PApplet.map(eg.enum,0,EventData.maxEventNumInGroup,6,17))
        text(eg.action,eg.pos.x,eg.pos.y)
      }
    }

    if(eventGroupsMap.get(ViewSet.selectedGroup) != None){
      val selectedGroup = eventGroupsMap(ViewSet.selectedGroup)
      fill(color(200,200,0),160)
      val r = selectedGroup.getDiameter()
      ellipse(selectedGroup.pos.x,selectedGroup.pos.y,r,r)
      if(ViewSet.isSelectLocking){
        noFill()
        stroke(255)
        ellipse(selectedGroup.pos.x,selectedGroup.pos.y,r*0.8f,r*0.8f)
        noStroke()
      }
      GlyphDrawer.draw(selectedGroup)
      fill(0)
      textSize(PApplet.map(selectedGroup.enum,0,EventData.maxEventNumInGroup,8,20))
      text(selectedGroup.action,selectedGroup.pos.x,selectedGroup.pos.y)
      textSize(10)
      NodeDetailView.drawGraph(selectedGroup)
    }
  }

  override def draw(){
    colorMode(PConstants.RGB)
    background(255,255,255)
    pushMatrix()
    drawChronoViewWindow()
    popMatrix()

    translate(ViewSet.r_view+50, ViewSet.r_view+50)
    colorMode(PConstants.RGB,255,255,255)

    EventData.eventGroups() match {
      case Some(eg) => drawEventGroups(eg)
      case None => return
    }
  }

  def drawChronoViewWindow() {
    translate(ViewSet.r_view+50, ViewSet.r_view+40)
    textSize(10);
    textAlign(PConstants.CENTER);
    stroke(0);
    fill(255,255,255);

    ellipse(0, 0, ViewSet.r_view * 2, ViewSet.r_view * 2);

    noStroke()
    colorMode(PConstants.HSB,24,100,100)
    for (h <- 0 to EventData.period-1) {
      val theta = ViewSet.getTheta(h, EventData.period);
      val x = ((ViewSet.r_view + 20) * Math.cos(theta)).asInstanceOf[Float];
      val y = ((ViewSet.r_view + 20) * Math.sin(theta)).asInstanceOf[Float];
      fill(color(h,100,100))
      text(h, x, y * -1);
    }
  }

  object GlyphDrawer{
    def draw(eg: EventGroup){
      pushMatrix()
      translate(eg.pos.x, eg.pos.y)

      drawCircleGlyph(eg)

      popMatrix()
    }

    def drawCircleGlyph(eg: EventGroup) {
      val r_max = eg.getDiameter();
      for (i <- 0 to EventData.period - 1) {
        val nr = r_max /2 * 0.5;
        val dx = nr * Math.cos(ViewSet.getTheta(i, EventData.period));
        val dy = nr * Math.sin(ViewSet.getTheta(i, EventData.period)) * -1;

        val count = eg.freqCount
        val m_i = eg.m_i
        if(count.size < 2) {
          return
        }

        val al = PApplet.map(count(i), 0, count(m_i), 0.4f, 0.8f);
        colorMode(PConstants.HSB,24,100,100)
        fill(color(i,100,100), 128);

        val mr = Math.sqrt(count(m_i) / PConstants.PI);
        val ar = Math.sqrt(count(i) / PConstants.PI) * ((0.3 * r_max/2) / mr);
        ellipse(dx.asInstanceOf[Float], dy.asInstanceOf[Float], ar.asInstanceOf[Float], ar.asInstanceOf[Float]);
        colorMode(PConstants.RGB,255,255,255)
      }
    }
  }

  object NodeDetailView{
    def drawGraph(eg: EventGroup){
      val eventsListByDay = eg.aggressionEventListByDay()
      val w = (350.0 / eventsListByDay.getAllDateCount().asInstanceOf[Float]).asInstanceOf[Float];
      val dateformat = DateFormat.getDateFormat(eventsListByDay.get(0).time)
      val sfd = new SimpleDateFormat("yyyy-MM-dd").format(eventsListByDay.firstDate(dateformat))
      val sld = new SimpleDateFormat("yyyy-MM-dd").format(eventsListByDay.lastDate( dateformat))

      pushMatrix();
      translate(300, 230);
      stroke(0, 128);
      line(0, 0, 350, 0);
      fill(0);
      text(sfd, 0  , 10);
      text(sld, 350, 10);
      drawCumulativeGraph(eg,eventsListByDay,w)

      //translate(0, -100);
      //fill(0);
     // text(sfd, 0  , 15);
     // text(sld, 350, 15);
      //drawStackedPanel(eg,eventsListByDay,w)
      popMatrix()
    }


    def drawCumulativeGraph(eg: EventGroup, eventsListByDay: EventsListByDay, w: Float){
      val sx = eventsListByDay.startDateArr
      val th = eventsListByDay.cumHeightArr

      (1 to eventsListByDay.size-1).foreach{ index =>
        val beforeHeight = PApplet.map(th(index-1), 0, eg.eventList.size()+5, 0, 80)
        strokeWeight(1.0f)
        stroke(0,64)
        line(sx(index-1).asInstanceOf[Int]*w, beforeHeight*(-1f), (sx(index).asInstanceOf[Int]-1f)*(w),beforeHeight*(-1f))

        strokeWeight(2.0f);
       // colorMode(PConstants.HSB,24,100,100)
        //stroke(DateFormat.dateformatOption(dateformat, eventsListByDay(index).time).get.getHours(),100,100)
        //colorMode(PConstants.RGB,255,255,255)
        val totalHeight = PApplet.map(th(index), 0, eg.eventList.size()+5, 0, 80)
        line((sx(index).asInstanceOf[Int]-1)*w, beforeHeight*(-1f), sx(index).asInstanceOf[Int]*w,totalHeight*(-1f))
      }
      strokeWeight(1.0f);
    }
    def drawStackedRiver(eg: EventGroup,eventsListByDay: EventsListByDay, w:Float){
      pushMatrix();
      translate(300, 0);
      stroke(0, 128);
      line(0, 0, 350, 0);

      val dateformat = DateFormat.getDateFormat(eventsListByDay.get(0).time)
      fill(0);
      text(new SimpleDateFormat("yyyy-MM-dd").format(eventsListByDay.firstDate(dateformat)), 0  , 10);
      text(new SimpleDateFormat("yyyy-MM-dd").format(eventsListByDay.lastDate( dateformat)), 350, 10);

      val sx = eventsListByDay.startDateArr
      val freq  = eventsListByDay.allDateFreq

      noStroke()
      (0 to EventData.period-1).foreach{ ii =>
        val i = 23 - ii
        beginShape()
        vertex(0,0)
        (1 to eventsListByDay.size-1).foreach{ j =>
          val dh = {
            if(i==0) {
              0
            }else{
              var d = 0
              (0 to i-1).foreach{ k=>
                d = d + freq(j)(k)*10
              }
              d
            }
          }
          colorMode(PConstants.HSB,24,100,100)
          fill(color(i,100,100), 255);

          if(freq(j)(i)!=0){
            if((sx(j).asInstanceOf[Int] - sx(j-1).asInstanceOf[Int]) >3) vertex((sx(j).asInstanceOf[Int]-3)*w,0)

            vertex(sx(j).asInstanceOf[Int]*w,-(freq(j)(i)*10 + dh))
          }

          colorMode(PConstants.RGB,255,255,255)
        }
        vertex(sx(eventsListByDay.size-1).asInstanceOf[Int]*w,0)
        endShape()
      }

      popMatrix();
      strokeWeight(1.0f);
    }

    def drawStackedPanel(eg: EventGroup,eventsListByDay: EventsListByDay, w:Float){
      val sx = eventsListByDay.startDateArr
      val freq  = eventsListByDay.allDateFreq

      noStroke()
      colorMode(PConstants.HSB,24,100,100)
      (0 to EventData.period-1).foreach{ i =>
        (1 to eventsListByDay.size-1).foreach{ j =>
          val yoko = (1 to eventsListByDay.size-1).map{ k =>
            freq(k)(i)
          }
          val alpha = 255 * PApplet.map(freq(j)(i),0,freq(j).max,0.0f,1.0f) * PApplet.map(freq(j)(i),0,yoko.max,0.0f,1.0f)
          if(alpha > 64){
            fill(color(i,100,100), alpha);
            rect(sx(j).asInstanceOf[Int]*w,-i*5,w*1.5f,5)
          }
        }
      }
      colorMode(PConstants.RGB,255,255,255)
    }

  }

}