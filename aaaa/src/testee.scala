import java.io.File
import java.io.FileWriter
import java.io.FileInputStream
import java.io.IOException
import java.security.MessageDigest

import org.apache.commons.io.FileUtils
import org.apache.commons.lang3.time.DateUtils
import scala.collection.JavaConversions._
import scala.collection.mutable
import java.awt._
import javax.swing._
import javax.swing.event.ListSelectionListener
import javax.swing.event.ListSelectionEvent
import javax.swing.event.ChangeListener
import javax.swing.event.ChangeEvent
import java.awt.Dimension
import java.awt.event._
import java.util.ArrayList
import java.util.Hashtable
import java.awt.Graphics2D
import processing.core.PApplet
import processing.core.PSurface
import processing.core.PConstants

import org.rosuda.JRI.REXP
import org.rosuda.JRI.Rengine
import java.text.SimpleDateFormat
import java.util.Date


class MyListSelectionListener(f :ListSelectionEvent => Unit) extends ListSelectionListener{
  override def valueChanged(event: ListSelectionEvent) = {f(event)}
}

class MyActionListener(f: ActionEvent => Unit) extends ActionListener{
  override def actionPerformed(event: ActionEvent) = { f(event) }
}

class MyChangeListener(f: ChangeEvent => Unit) extends ChangeListener{
  override def stateChanged(event: ChangeEvent) = { f(event) }
}


object Window2{
  def main(args: Array[String]){
    val frame = new JFrame();
    frame.setLayout(new BorderLayout())
    frame.setSize(1200,700);
    val second = new Applet(frame);

    val menubar = new JMenuBar()
   // menubar.add(Box.createHorizontalGlue())
    val menuFile = new JMenu("ファイル")
    val menuData = new JMenu("データ")
    menubar.add(menuFile)
    menubar.add(menuData)

    val menuNew = new JMenuItem("新規")
    val menuOpen = new JMenuItem("開く")
    val menuClose = new JMenuItem("終了")

    menuFile.add(menuNew)
    menuFile.add(menuOpen)
    menuFile.add(menuClose)

    val menuMDS = new JMenu("MDS")
    val menuDTW = new JMenuItem("DTW")
    val menuDFM = new JMenuItem("eades適用")
    val menuCluster = new JMenuItem("クラスタリング")
    menuData.add(menuMDS)
    menuData.add(menuDTW)
    menuData.add(menuDFM)
    menuData.add(menuCluster)
    frame.setJMenuBar(menubar)

    val menuEuclidean = new JMenuItem("euclidean")
    val menuManhattan = new JMenuItem("manhattan")
    val menuCanberra = new JMenuItem("canberra")
    val menuPearson = new JMenuItem("pearson")
    val menuCorrelation = new JMenuItem("correlation")

    menuMDS.add(menuEuclidean)
    menuMDS.add(menuManhattan)
    menuMDS.add(menuCanberra)
    menuMDS.add(menuPearson)
    menuMDS.add(menuCorrelation)

    val centerPanel = new JPanel()
    centerPanel.setSize(10,10)
    frame.add(centerPanel, BorderLayout.EAST)

    val colListPanel = new JPanel()
    colListPanel.setSize(120,700)
    colListPanel.setLayout(new BorderLayout)
    frame.add(colListPanel,BorderLayout.WEST)

    val colList = new JList(new Array[String](1))
    colList.setFixedCellWidth(120)
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

    val periodSlider = new JSlider(1,4)
    periodSlider.addChangeListener(new MyChangeListener (event => {

    }))
    periodSlider.setMajorTickSpacing(1)
    periodSlider.setPaintTicks(true)

    val periodlabels = new Hashtable[Integer, JComponent]()
    periodlabels.put(new Integer(1), new JLabel("1day"))
    periodlabels.put(new Integer(2), new JLabel("1week"))
    periodlabels.put(new Integer(3), new JLabel("1month"))
    periodlabels.put(new Integer(4), new JLabel("1year"))
    periodSlider.setLabelTable(periodlabels)

    p.add(periodSlider)

    val gravitySlider = new JSlider(0,100)
    val g_clockSlider = new JSlider(0,100)

    gravitySlider.setValue(10)
    g_clockSlider.setValue(15)
    p.add(gravitySlider)
    p.add(g_clockSlider)

    frame.add(bottomPanel, BorderLayout.SOUTH)

    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.setVisible(true);

    val framedtw = new JFrame();
    framedtw.setLayout(new BorderLayout())
    framedtw.setSize(370,580);
    framedtw.setVisible(true);
    val p3dtw = new DTWViewApplet(framedtw);


    button.addActionListener(new MyActionListener (event => {
      val index = colList.getSelectedIndex()
      EventData.changeEventGroups(index)
      eventGroupList.setListData(EventData.eventGroupNames())
    }))

    menuDTW.addActionListener(new MyActionListener (event => {

      val mdsposition = EventData.DTWdistMDS()
      EventData.setMDSPosition(mdsposition._1, mdsposition._2)
    }))

    menuCluster.addActionListener(new MyActionListener (event => {
      val ambigroup = EventData.ambiguousGroups()
      val graph = new Graph(40,ambigroup.size,ambigroup)
      EventData.clustering()
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
          EventData.makeEventGroups(0, opened._2)
          colList.setListData(opened._1)
          eventGroupList.setListData(EventData.eventGroupNames())
          frame.show()
          //EventData.printfreq()
        }else{
          println("open error")
        }
      }
    }))

    menuEuclidean.addActionListener(new MyActionListener( event => {
      val mdsposition = JavaToRCooperator.mdsAndProcrustesRotation("euclidean")
      EventData.setMDSPosition(mdsposition._1, mdsposition._2)
    }))
    menuManhattan.addActionListener(new MyActionListener( event => {
      val mdsposition = JavaToRCooperator.mdsAndProcrustesRotation("manhattan")
      EventData.setMDSPosition(mdsposition._1, mdsposition._2)
    }))
    menuCanberra.addActionListener(new MyActionListener( event => {
      val mdsposition = JavaToRCooperator.mdsAndProcrustesRotation("canberra")
      EventData.setMDSPosition(mdsposition._1, mdsposition._2)
    }))
    menuPearson.addActionListener(new MyActionListener( event => {
      val mdsposition = JavaToRCooperator.mdsAndProcrustesRotation("pearson")
      EventData.setMDSPosition(mdsposition._1, mdsposition._2)
    }))
    menuCorrelation.addActionListener(new MyActionListener( event => {
      val mdsposition = JavaToRCooperator.mdsAndProcrustesRotation("correlation")
      EventData.setMDSPosition(mdsposition._1, mdsposition._2)
    }))

    menuDFM.addActionListener(new MyActionListener( event => {
      val ambigroup = EventData.ambiguousGroups()
      val graph_eades = new GraphEades(30f,gravitySlider.getValue()/100f,g_clockSlider.getValue()/1000f,40,ambigroup.size,ambigroup)
      val newpositions = graph_eades.draw()
      ViewSet.points = graph_eades.ccc();

      EventData.setPosition(newpositions)
    }))
  }

  def isCsvFile(f: File): Boolean = f.isFile() && f.canRead() && f.getPath().endsWith(".csv")

}

object JavaToRCooperator{

  def DTWdist(arr: Array[Array[Double]], i: Int) : Array[Float] = {
    val engine = if(Rengine.getMainEngine !=null) Rengine.getMainEngine else new Rengine(Array("--no-save"), false, null)
    engine.eval("library(dtw)")

    val dist = new Array[Float](arr.size)

    engine.assign("a",arr(i))
    engine.eval("av <- as.vector(a)")
    engine.eval("alen <- length(av)")
    (0 to arr.size-1).foreach{ j=>
      if(i == j){
        dist(j) = 1000000
      }else{
        engine.assign("b",arr(j))
        engine.eval("bv <- as.vector(b)")
        engine.eval("blen <- length(bv)")
        val d = engine.eval("dtw(av, bv, step.pattern = symmetric1, window.type='sakoechiba', window.size = abs(alen - blen), distance.only=TRUE)$distance").asDouble.asInstanceOf[Float]
        dist(j) = d
      }
    }
    return dist
  }

  def DTWdistMDS(arr: Array[Array[Double]]) : (Array[Double],Array[Double]) = {
    val clearwriter = new FileWriter(new File("dtwdist.csv"), false);
    clearwriter.write("")
    clearwriter.close()

    val engine = if(Rengine.getMainEngine !=null) Rengine.getMainEngine else new Rengine(Array("--no-save"), false, null)

    engine.eval("library(dtw)")

    val dist = Array.ofDim[Float](arr.size,arr.size)

    (0 to arr.size-2).foreach{ i=>
      engine.assign("a",arr(i))
      engine.eval("av <- as.vector(a)")
      engine.eval("alen <- length(av)")
      (i+1 to arr.size-1).foreach{ j=>
        engine.assign("b",arr(j))
        engine.eval("bv <- as.vector(b)")
        engine.eval("blen <- length(bv)")
        val d = engine.eval("dtw(av, bv, step.pattern = symmetric1, window.type='sakoechiba', window.size = abs(alen - blen), distance.only=TRUE)$distance").asDouble.asInstanceOf[Float]
        dist(i)(j) = d
        dist(j)(i) = d
      }
    }

    val filewriter = new FileWriter(new File("dtwdist.csv"), false);
    (0 to arr.size-1).foreach{ i=>
      (0 to arr.size-1).foreach{ j=>
        filewriter.write(dist(i)(j)+",")
      }
      filewriter.write("\n")
    }
    filewriter.close()

    val commands = Array("X <- read.csv('dtwdist.csv',header=F)","data <- X[,1:ncol(X)-1]")
    commands.foreach{ s =>
      engine.eval(s)
    }
    engine.eval("cmd <- as.array(cmdscale(data))")
    engine.eval("points = matrix(0, nrow = nrow(X), ncol = 2)")
    val pos = EventData.chronoPositions
    val cpx = pos.map(p => p.x.asInstanceOf[Double]).toArray
    val cpy = pos.map(p => p.y.asInstanceOf[Double]).toArray

    engine.assign("cx",cpx)
    engine.assign("cy",cpy)
    engine.eval("points[,1] <- cx")
    engine.eval("points[,2] <- cy")
    engine.eval("library(MASS)")
    engine.eval("library(vegan)")
    engine.eval("proc <- procrustes(points,cmd,scale=TRUE)")

    val x = engine.eval("proc$Yrot[,1]").asDoubleArray()
    val y = engine.eval("proc$Yrot[,2]").asDoubleArray()
    engine.end()
    return (x, y)
  }

  def mdsAndProcrustesRotation(disttype: String):(Array[Double],Array[Double]) = {
    val flist:Array[Array[Int]] = EventData.frequencyVectors()
    val clearwriter = new FileWriter(new File("flisttmp.csv"), false);
    clearwriter.write("")
    clearwriter.close()

    val filewriter = new FileWriter(new File("flisttmp.csv"), true);

    EventData.ambiguousGroups.foreach{ eg =>
      val gf = eg.freqCount
      gf.foreach{ f=>
        filewriter.write(f+",");
      }
      filewriter.write("\n")
    }
    filewriter.close()

    val engine = if(Rengine.getMainEngine !=null) Rengine.getMainEngine else new Rengine(Array("--no-save"), false, null)
    val commands = Array("library(amap)","X <- read.csv('flisttmp.csv',header=F)","data <- X[,1:24]")
    commands.foreach{ s =>
      engine.eval(s)
    }
    engine.eval("cmd <- as.array(cmdscale(Dist(data,method=\""+disttype+"\")))")
    engine.eval("points = matrix(0, nrow = nrow(X), ncol = 2)")
    val pos = EventData.chronoPositions
    val cpx = pos.map(p => p.x.asInstanceOf[Double]).toArray
    val cpy = pos.map(p => p.y.asInstanceOf[Double]).toArray

    engine.assign("cx",cpx)
    engine.assign("cy",cpy)
    engine.eval("points[,1] <- cx")
    engine.eval("points[,2] <- cy")
    engine.eval("library(MASS)")
    engine.eval("library(vegan)")
    engine.eval("proc <- procrustes(points,cmd,scale=TRUE)")

    val x = engine.eval("proc$Yrot[,1]").asDoubleArray()
    val y = engine.eval("proc$Yrot[,2]").asDoubleArray()
    engine.end()
    return (x, y)
  }
}
