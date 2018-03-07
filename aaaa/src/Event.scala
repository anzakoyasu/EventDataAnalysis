import java.util.Date
import processing.core.PApplet
import processing.core.PConstants
import java.text.SimpleDateFormat
import scala.collection.mutable
import scala.collection.immutable
import scala.collection.JavaConversions._
import java.util.{ArrayList,Collections,Comparator,List=>JList}


case class Event(time: String, record: Array[String]){
}

case class EventsByDay(time: String, action: String, cumDate: Int) extends ArrayList[Event]{
  def lastDate():Event = get(size()-1)

  def freq():Array[Int] = {
    val freq = new Array[Int](EventData.period)
    val dateformat = DateFormat.getDateFormat(get(0).time)
    (0 to size - 1).foreach { i =>
      val h = (DateFormat.dateformatOption(dateformat, get(i).time).get).getHours()
      freq.update(h,freq(h) + 1)
    }
    return freq
  }
}

case class EventsListByDay() extends ArrayList[EventsByDay]{
  def getAllDateCount():Int = get(size-1).cumDate

  def firstDate(f: SimpleDateFormat):Date = DateFormat.dateformatOption(f, get(0).time).get

  def lastDate( f: SimpleDateFormat):Date = DateFormat.dateformatOption(f, get(size()-1).lastDate().time).get

  def getDate(f: SimpleDateFormat, d: Int):Date = DateFormat.dateformatOption(f, get(d).time).get

  def getFreq(d: Int):Int = get(d).size()

  def cumHeightArr:Array[Float] = {
    val th = new Array[Float](size)
    (0 to size-1).map{ i =>
      th(i) = {if(i == 0) get(0).size() else th(i-1) + get(i).size()}
    }
    return th
  }

  def allDateFreq(): Array[Array[Int]] = {
    val ite = (0 to size - 1).map { i =>
      get(i).freq
    }
    return ite.toArray[Array[Int]]
  }

  def startDateArr:IndexedSeq[AnyVal] = (0 to size-1).map{ i => if(i == 0) 0 else get(i).cumDate }

  def timeSeriesArray(bin:Int) : Array[Double] = {
    val arrlist = new ArrayList[Float]
    val allDayFreq = allDateFreq()

    allDayFreq.foreach{ freqHours =>
      var c = 0
      var f = 0
      freqHours.foreach{ freq =>
        c = c + 1
        f = f + freq
        if(c%bin == 0){
           arrlist.add(f/bin)
           f = 0
        }
      }
    }
    val arr = new Array[Double](arrlist.size)
    (0 to arr.size - 1).foreach{ i=>
      arr(i) = arrlist.get(i).asInstanceOf[Double]
    }

    return arr
  }
}


class EventGroupOnCircum(eventList: Vector[Event], action: String, pos: Position) extends EventGroup(eventList, action, pos){
  override val isLocatedOnCircum = true
}

class EventGroupCluster(eventList: Vector[Event], action: String, pos: Position) extends EventGroup(eventList, action, pos){

}

case class EventGroup(eventList: Vector[Event], action: String, pos: Position){
  val isLocatedOnCircum = false

  var freqCount = new Array[Int](1)
  var m_i = 0

  def add(event: Event) = this.copy(eventList = eventList :+ event)

  def get(index: Int): Event ={
    return eventList(index)
  }

  def positionChange(newPos: Position) = this.copy(pos = newPos)

  def enum(): Int = {
    return eventList.length
  }

  def setFreqCount(){
    val f = frequencyInEachTime(EventData.period)
    freqCount = f._1
    m_i = f._2
  }

  def frequencyInEachTime(period: Int) : (Array[Int], Int) = {
    val array = new Array[Int](period)
    val dateformat = DateFormat.getDateFormat(eventList(0).time)

    eventList.foreach { event =>
      val h = (DateFormat.dateformatOption(dateformat, event.time).get).getHours()
      array.update(h,array(h)+1)
    }

    (array, array.indexOf(array.max))
  }

  def aggressionEventListByDay():EventsListByDay = {
    val eventsList = new EventsListByDay()
    var lastevents = new EventsByDay(eventList(0).time, action, 0)
    lastevents.add(eventList(0))

    val dateformat = DateFormat.getDateFormat(eventList(0).time)
    var lastdate = DateFormat.dateformatOption(dateformat, eventList(0).time).get

    for(i <- 1 to eventList.length - 1){
      val event = eventList(i)
      val ndate = DateFormat.dateformatOption(dateformat, event.time).get
      val lastM = lastdate.getMonth()
      val lastD = lastdate.getDate()
      if(lastD != ndate.getDate() || lastM != ndate.getMonth()){
        eventsList.add(lastevents)

        val dateTimeTo = ndate.getTime()
        val dateTimeFrom = lastdate.getTime()
        val diff = ((dateTimeTo - dateTimeFrom) / (1000*60*60*24).asInstanceOf[Float]).asInstanceOf[Float]
        val additionalDate = if(diff >= 1) diff else 1
        lastevents = new EventsByDay(event.time, action, (lastevents.cumDate+ additionalDate.asInstanceOf[Int]))
        lastevents.add(event)
        lastdate = ndate
      }else{
        lastevents.add(event)
      }
    }
    eventsList.add(lastevents)
    return eventsList
  }

  def chronoPoint(): Position = {
    var newx = 0f
    var newy = 0f
    val dateformat = DateFormat.getDateFormat(eventList(0).time)

    eventList.foreach { event =>
      val date = DateFormat.dateformatOption(dateformat, event.time).get
      val h = DateFormat.getHours(date) + PApplet.map(DateFormat.getMinutes(date),0,59,0.0f,0.99f);
      val theta = ViewSet.getTheta(h, EventData.period);
      newx = newx + ViewSet.r_view * Math.cos(theta).asInstanceOf[Float]
      newy = newy + ViewSet.r_view * Math.sin(theta).asInstanceOf[Float]
    }

    newx /= eventList.length;
    newy /= eventList.length;
    newy *= -1

    return new Position(newx, newy)
  }

  def getDiameter():Float =  {
    val ar = 2 * Math.sqrt(eventList.length / PConstants.PI);
    val mr = Math.sqrt(EventData.maxEventNumInGroup / PConstants.PI);
    val area = ar * (0.09 * ViewSet.r_view) / mr;

    return 5f + area.asInstanceOf[Float];
  }

}

object EventData{
  val groups = mutable.Map.empty[String, EventGroup]
  val period = 24//時機に毎度スライダーから取得するようにする

  def DTWdist(egstr: String): Array[Float]= {
    val arrEvDay = groups.values.map{ gi =>
      gi.aggressionEventListByDay().timeSeriesArray(3)
    }.toArray[Array[Double]]

    val index = groups.keys.zipWithIndex.map{ keyi =>
      if(keyi._1 == egstr) keyi._2
      else 0
    }.foldLeft(0)(_ + _)
    println(egstr)
    return JavaToRCooperator.DTWdist(arrEvDay, index)
  }

  def DTWdistMDS(): (Array[Double],Array[Double])= {
    val arrEvDay = groups.values.map{ gi =>
      gi.aggressionEventListByDay().timeSeriesArray(3)
    }.toArray[Array[Double]]

    return JavaToRCooperator.DTWdistMDS(arrEvDay)
  }

  def eventGroups():Option[mutable.Map[String, EventGroup]] = {
    if(groups.size == 0) return None
    else                 return Some(groups)
  }

  def frequencyVectors():Array[Array[Int]] = {
    return groups.values.map{ _.freqCount }.toArray
  }

  def maxEventNumInGroup():Int = {
    try{
      groups.values.map{ _.eventList.size }.max
    }catch{
      case e: Exception => 0
    }
  }

  def eventGroupNames() = groups.values.map{ _.action }.toArray[String]

  def makeEventGroups(col_event:Int, loaded:ArrayList[String]){
    groups.clear()
    loaded.foreach{ row =>
      val record = row split ","
      val time   = record(2).replaceAll("T"," ")
      addEvent(record(col_event), new Event(time, record))
    }
    calcPositionOfEventGroups()
    setFreqCount()
  }

  def changeEventGroups(col_event:Int){
    val array = new ArrayList[Event]
    groups.values.foreach{ _.eventList.map{ event => array.add(event) }}
    val loaded = sortByDate(array)

    groups.clear()
    loaded.foreach{ row => addEvent(row.record(col_event), new Event(row.time, row.record)) }
    calcPositionOfEventGroups()
    setFreqCount()
  }

  def sortByDate(list: JList[Event]):ArrayList[Event] = {
    val r = new ArrayList[Event](list)
    val date = r(1).time
    val dateFormat = DateFormat.getDateFormat(date)

    Collections.sort(r, new Comparator[Event]{
      override def compare(o1: Event, o2:Event): Int = {
        val o1time:Date = (DateFormat.dateformatOption(dateFormat, o1.time )).get
        val o2time:Date = (DateFormat.dateformatOption(dateFormat, o2.time )).get
        o1time.getTime().compareTo(o2time.getTime())
      }
    })
    return r
  }

  def isMeet():Boolean = {
    val egarray = groups.values.toArray[EventGroup]
    (0 to egarray.size-2).foreach( i =>{
      val p1 = egarray(i).pos
      (i+1 to egarray.size-1).foreach( j =>{
        val p2 = egarray(j).pos
        if(shouldCluster(p1,p2)){
          return true
        }
      })
    })
    return false
  }

  def shouldCluster(p1: Position, p2:Position): Boolean = {
    if(Math.sqrt((p1.x-p2.x)*(p1.x-p2.x)+(p1.y-p2.y)*(p1.y-p2.y)) < 30){
          return true
    }
    return false
  }

  def clustering(){
    while(isMeet()){
      var a = false
      (0 to groups.size-2).foreach( i =>{
        (i+1 to groups.size-1).foreach( j => {
          if(a == false){
            val egarray = groups.values.toArray[EventGroup]
            val eg1 = egarray(i)
            val eg2 = egarray(j)
            if(shouldCluster(eg1.pos, eg2.pos)){
              val newEventList = eg1.eventList.map(ev=> ev).toVector ++: eg2.eventList.map(ev=>ev).toVector
              val newAction = eg1.action +"," +eg2.action
              groups += (newAction -> new EventGroupCluster(newEventList, newAction, eg1.pos))
              groups.remove(eg1.action)
              groups.remove(eg2.action)
              a = true
            }
          }
        })
      })
    }
    setFreqCount()
  }

  def addEvent(action: String, event: Event){
    if(groups.contains(action)){
      groups(action) = groups(action).add(event)//copyを再代入
    }else{
      val newGroup = new EventGroup(Vector(event),action, new Position(0,0))
      groups += (action -> newGroup)
    }
  }

  def calcPositionOfEventGroups() {
    groups.values.foreach{ eg =>
      val chronoPosition = eg.chronoPoint()
      if(chronoPosition.isLocatedOnCircum()){
        groups(eg.action) = new EventGroupOnCircum(eg.eventList, eg.action, chronoPosition)
      }else{
        groups(eg.action) = eg.positionChange(chronoPosition)
      }
    }
  }

  def setMDSPosition(x: Array[Double],y: Array[Double]) {
    val fx = (0 to x.size - 1).map{j => x(j).asInstanceOf[Float]}.toArray
    val fy = (0 to y.size - 1).map{j => y(j).asInstanceOf[Float]}.toArray

    MDSPositions.clear
    val alpha= 1.0f

    ambiguousGroups().zipWithIndex.foreach{ tuple =>
      val newx = PApplet.map(fx(tuple._2),PApplet.min(fx),PApplet.max(fx),-ViewSet.r_view *(alpha),ViewSet.r_view *(alpha))
      val newy = PApplet.map(fy(tuple._2),PApplet.min(fy),PApplet.max(fy),-ViewSet.r_view *(alpha),ViewSet.r_view *(alpha))
      MDSPositions.add(new MDSPos(newx,newy))
      groups(tuple._1.action) = tuple._1.positionChange(new Position(newx,newy))
    }
    setFreqCount()
  }

  def setPosition(pos: Array[Position]):Unit = {
    ambiguousGroups().zipWithIndex.foreach{ tuple =>
      groups(tuple._1.action) = tuple._1.positionChange(pos(tuple._2))
    }
    setFreqCount()
  }

  def ambiguousGroups():Iterable[EventGroup]={
    groups.filter(_._2.isLocatedOnCircum == false).values()
  }

  def setFreqCount(): Unit = groups.foreach { _._2.setFreqCount() }

  def chronoPositions(): Array[Position] = {
    return ambiguousGroups().map{ _.chronoPoint() }.toArray
  }

  def xarray() = ambiguousGroups().map{ _.pos.x.asInstanceOf[Double] }.toArray[Double]
  def yarray() = ambiguousGroups().map{ _.pos.y.asInstanceOf[Double] }.toArray[Double]

 }