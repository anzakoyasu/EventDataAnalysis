import java.io.File
import java.io.FileInputStream
import java.io.IOException
import java.security.MessageDigest

import org.apache.commons.io.FileUtils
import org.apache.commons.lang3.time.DateUtils
import scala.collection.JavaConversions._
import java.awt._
import javax.swing._
import javax.swing.event.ListSelectionListener
import javax.swing.event.ListSelectionEvent
import javax.swing.event.ChangeListener
import javax.swing.event.ChangeEvent
import java.awt.Dimension
import java.awt.event._
import java.util.ArrayList
import java.awt.Graphics2D
import processing.core.PApplet
import processing.core.PSurface
import processing.core.PConstants

class MyListSelectionListener(f :ListSelectionEvent => Unit) extends ListSelectionListener{
  override def valueChanged(event: ListSelectionEvent) = {f(event)}
}

class MyActionListener(f: ActionEvent => Unit) extends ActionListener{
  override def actionPerformed(event: ActionEvent) = { f(event) }
}

class MyChangeListener(f: ChangeEvent => Unit) extends ChangeListener{
  override def stateChanged(event: ChangeEvent) = { f(event) }
}

object ViewSet{
  val r_view =230

  val gcl_h = Array(16331852,16468280,16606765,16744740,16751381,16758301,16765222,16772399,
                    14280239,11392812,8964154,7060043,4697177,3122285,2269830,209998,
                    2199205,2523310,3567792,6186670,8675756,10902185,12735647,14437781)

  def getTheta(t: Float, period: Float): Float = {
    return PConstants.PI/2 - 2*PConstants.PI*(t/period);
  }

  var selectedGroup:String = ""
  var display_glyph = true
  var display_label = true

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
  f.add(panel,BorderLayout.WEST);
  //this.showSurface()
  this.startSurface();

  override def settings() {
    size(1000, 700)
    smooth(8);
  }
  override def setup(){
    background(255, 255, 255);

  }

  override def draw(){
    colorMode(PConstants.RGB)
    background(255,255,255)
    pushMatrix()
    drawChronoViewWindow()
    popMatrix()

    val eventGroupsOption = EventData.eventGroups()
    eventGroupsOption match {
      case Some(eg) => {
      }
      case None => return
    }

    val eventGroups = eventGroupsOption.get
    //描画start
    translate(ViewSet.r_view+50, ViewSet.r_view+50)
    colorMode(PConstants.RGB,255,255,255)
    fill(color(200),64)

    eventGroups.foreach{ map =>
      val eventGroup:EventGroup = map._2
      val r = eventGroup.getDiameter()
      if(eventGroup.eventList(0).action != ViewSet.selectedGroup){
        ellipse(eventGroup.x,eventGroup.y,r,r)
      }
    }

    eventGroups.foreach{ map =>
      if(map._2.eventList(0).action != ViewSet.selectedGroup && ViewSet.display_glyph){
        GlyphDrawer.draw(map._2)
      }
    }

    fill(0)
    eventGroups.foreach{ map =>
      val eventGroup:EventGroup = map._2
      if(eventGroup.eventList(0).action != ViewSet.selectedGroup && ViewSet.display_label){
        text(eventGroup.eventList(0).action,eventGroup.x,eventGroup.y)
      }
    }

    if(eventGroups.get(ViewSet.selectedGroup) != None){
      val selectedGroup = eventGroups.get(ViewSet.selectedGroup).get
      fill(color(200,200,0),160)
      val r = selectedGroup.getDiameter()
      ellipse(selectedGroup.x,selectedGroup.y,r,r)
      GlyphDrawer.draw(selectedGroup)
      fill(0)
      text(selectedGroup.eventList(0).action,selectedGroup.x,selectedGroup.y)

      NodeDetailView.drawCumulativeGraph(eventGroups(ViewSet.selectedGroup))
    }

  }

  def drawChronoViewWindow() {
    translate(ViewSet.r_view+50, ViewSet.r_view+40)
//    fill(0,0,0,64)
//    rect(-ViewSet.r_view-50,-ViewSet.r_view-50,ViewSet.r_view*2+100,ViewSet.r_view*2+80)
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
      translate(eg.x, eg.y)

      drawCircleGlyph(eg)

      popMatrix()
    }

    def drawCircleGlyph(eg: EventGroup) {
      val r_max = eg.getDiameter();
      for (i <- 0 to EventData.period - 1) {
        val nr = r_max / 6;
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

        var ar = Math.sqrt(count(i) / PConstants.PI);
        val mr = Math.sqrt(count(m_i) / PConstants.PI);
        ar =  ar * ((0.3 * r_max/2) / mr);
        ellipse(dx.asInstanceOf[Float], dy.asInstanceOf[Float], ar.asInstanceOf[Float], ar.asInstanceOf[Float]);
        colorMode(PConstants.RGB,255,255,255)
      }
    }
  }

  object NodeDetailView{
    def drawCumulativeGraph(eg: EventGroup){
      val eventsListByDay = eg.aggressionEventListByDay();
      val w = (350.0 / eventsListByDay.getAllDateCount().asInstanceOf[Float]).asInstanceOf[Float];

      pushMatrix();
      translate(300, 150);
      stroke(0, 128);
      line(0, 0, 350, 0);
      fill(0);

      val dateformat = DateFormat.getDateFormat(eventsListByDay.get(0).time)
      val lf = eventsListByDay.firstDate(dateformat)
      val ld = eventsListByDay.lastDate(dateformat)

      import java.text.SimpleDateFormat
      import java.util.Date
      text(new SimpleDateFormat("yyyy-MM-dd").format(lf), 0  , 10);
      text(new SimpleDateFormat("yyyy-MM-dd").format(ld), 350, 10);

      var totalHeight = 0.0f;
      var total = 0.0f
      var d = 0
      eventsListByDay.foreach{ events =>
        val tmpDate:Date = DateFormat.dateformatOption(dateformat, events.time).get
        val frq = events.size()

        val beforeHeight = totalHeight

        strokeWeight(1.0f)
        stroke(0,64)

        line((d)*(w), beforeHeight*(-1f),(d+ events.interval-1)*w,totalHeight*(-1f))
        total = total + frq
        totalHeight = PApplet.map(total, 0, eg.eventList.size()+5, 0, 100);

        strokeWeight(2.0f);
        colorMode(PConstants.HSB,24,100,100)
        stroke(tmpDate.getHours(),100,100)
        colorMode(PConstants.RGB,255,255,255)

        line((d + events.interval-1)*(w), beforeHeight*(-1f),(d + events.interval)*w,totalHeight*(-1f))
        d = d + events.interval
      }
      popMatrix();
      strokeWeight(1.0f);
    }
  }

}

object Window2{
  def main(args: Array[String]){
    val menubar = new JMenuBar()
    val menuFile = new JMenu("ファイル")
    menubar.add(menuFile)

    val menuNew = new JMenuItem("新規")
    val menuOpen = new JMenuItem("開く")
    val menuClose = new JMenuItem("終了")

    menuFile.add(menuNew)
    menuFile.add(menuOpen)
    menuFile.add(menuClose)

    val frame = new JFrame();
    frame.setLayout(new BorderLayout())
    frame.setSize(1200,700);
    val second = new Applet(frame);
    frame.setJMenuBar(menubar)

    val centerPanel = new JPanel()
    centerPanel.setSize(10,10)
    frame.add(centerPanel, BorderLayout.CENTER)

    val colListPanel = new JPanel()
    colListPanel.setLayout(new BorderLayout)
    frame.add(colListPanel,BorderLayout.EAST)

    val colList = new JList(EventData.cols)
    colListPanel.add(colList,BorderLayout.CENTER)
    val button = new JButton("表示切替")
    colListPanel.add(button,BorderLayout.SOUTH)

    val eventGroupList = new JList(EventData.eventGroupNames())
    eventGroupList.addListSelectionListener(new MyListSelectionListener (event => {
      ViewSet.selectedGroup = eventGroupList.getSelectedValue()
    }))

    val bottomPanel = new JPanel()
    val p = new JPanel()
    val sp = new JScrollPane()
    sp.getViewport().setView(eventGroupList)
    sp.setPreferredSize(new Dimension(400,100))
    bottomPanel.setLayout(new GridLayout(1,2))
    bottomPanel.add(sp)
    bottomPanel.add(p)

    val cb1 = new JCheckBox("display Glyph")
    cb1.addChangeListener(new MyChangeListener (event => {
      if(cb1.isSelected()){
        ViewSet.display_glyph = true
      }else{
        ViewSet.display_glyph = false
      }
    }))
    cb1.setSelected(true)
    p.add(cb1)

    val cb2 = new JCheckBox("display Label")
    cb2.addChangeListener(new MyChangeListener (event => {
      if(cb2.isSelected()){
        ViewSet.display_label = true
      }else{
        ViewSet.display_label = false
      }
    }))
    cb2.setSelected(true)
    p.add(cb2)

    frame.add(bottomPanel, BorderLayout.SOUTH)

    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.setVisible(true);

    button.addActionListener(new MyActionListener (event => {
      val index = colList.getSelectedIndex()
      EventData.changeEventGroups(index)
      eventGroupList.setListData(EventData.eventGroupNames())
    }))

    menuOpen.addActionListener(new MyActionListener (event => {
      val fc = new JFileChooser()
      fc.setDialogTitle("ファイル選択")

      val selected = fc.showOpenDialog(frame)
      if(selected == JFileChooser.APPROVE_OPTION){
        val importFile:File = fc.getSelectedFile()
        if(isCsvFile(importFile)){
          val opened = FileLoader.fileOpen(importFile)
          EventData.makeEventGroups(0,opened._1,opened._2)
          colList.setListData(EventData.cols)
          eventGroupList.setListData(EventData.eventGroupNames())
          frame.show()

        }else{
          println("open error")
        }
      }
    }))

  }

  def isCsvFile(f: File): Boolean = f.isFile() && f.canRead() && f.getPath().endsWith(".csv")

}
