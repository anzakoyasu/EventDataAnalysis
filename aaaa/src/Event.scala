import java.util.ArrayList
import java.util.Date
import processing.core.PApplet
import processing.core.PConstants
import java.text.SimpleDateFormat

case class Event(time: String, action: String, other:Array[String]){
}


case class EventGroup(eventList: Vector[Event], x: Float, y: Float){
  var freqCount = new Array[Int](1)
  var m_i = 0

  case class EventsByDay(time: String, action: String, interval: Int) extends ArrayList[Event]{
    def lastDate():Event = {
      return get(size()-1)
    }
  }

  case class EventsListByDay() extends ArrayList[EventsByDay]{
    def getAllDateCount():Int = {
      var count = 1
      for(i <- 0 to size() - 1){
        count = count + get(i).interval
      }
      return count
    }

    def firstDate(f: SimpleDateFormat):Date ={
      DateFormat.dateformatOption(f, get(0).time).get
    }
    def lastDate(f: SimpleDateFormat):Date ={
      DateFormat.dateformatOption(f, get(size()-1).lastDate().time).get
    }

    def getDate(f: SimpleDateFormat, d: Int):Date ={
      DateFormat.dateformatOption(f, get(d).time).get
    }

    def getFreq(d: Int):Int = {
      return get(d).size()
    }
  }

  def setFreqCount(){
    val f = frequencyInEachTime(EventData.period)
    freqCount = f._1
    m_i = f._2
  }

  def add(event: Event) = this.copy(eventList = eventList :+ event)

  def positionChange(newx: Float, newy: Float) = this.copy(x = newx, y = newy)

  def frequencyInEachTime(period: Int) : (Array[Int], Int) = {
    val array = new Array[Int](period)
    val dateformat = DateFormat.getDateFormat(eventList(0).time)

    eventList.foreach { event =>
      val h = (DateFormat.dateformatOption(dateformat, event.time).get).getHours()
      array.update(h,array(h)+1)
    }

    var max = 0
    var m_ic = 0

    array.zipWithIndex.foreach { c =>
      if(max < c._1){
        max = c._1
        m_ic = c._2
      }
    }
    (array, m_ic)
  }

  def aggressionEventListByDay():EventsListByDay = {
    val eventsList = new EventsListByDay()

    var lastevents = new EventsByDay(eventList(0).time,eventList(0).action,0)
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
        var diff = ((dateTimeTo - dateTimeFrom) / (1000*60*60*24).asInstanceOf[Float]).asInstanceOf[Float]
        if(diff < 1){
          diff = 1
        }
        lastevents = new EventsByDay(event.time,event.action,diff.asInstanceOf[Int])
        lastevents.add(event)
        lastdate = ndate
      }else{
        lastevents.add(event)
      }
    }
    eventsList.add(lastevents)
    return eventsList
  }

  def calcPoint():(Float, Float) = {
    var newx = 0f
    var newy = 0f
    val dateformat = DateFormat.getDateFormat(eventList(0).time)

    eventList.foreach { event =>
      val date = DateFormat.dateformatOption(dateformat, event.time).get
      val h = getHoursFromDate(date) + PApplet.map(getMinutesFromDate(date),0,59,0.0f,0.99f);
      val theta = ViewSet.getTheta(h, EventData.period);
      newx = newx + ViewSet.r_view * Math.cos(theta).asInstanceOf[Float]
      newy = newy + ViewSet.r_view * Math.sin(theta).asInstanceOf[Float]
    }

    newx /= eventList.length;
    newy /= eventList.length;
    newy *= -1

    return (newx, newy)

  }

  def getDiameter():Float =  {
    val ar = 2 * Math.sqrt(eventList.length / PConstants.PI);
    val mr = Math.sqrt(300 / PConstants.PI);
    val area = ar * (0.1 * ViewSet.r_view) / mr;

    return 15f + area.asInstanceOf[Float];
  }


  def getHoursFromDate(d: Date):Int =  {
    return  (d.getHours() % EventData.period).asInstanceOf[Int];
  }

  def getMinutesFromDate(d: Date):Float = {
    return d.getMinutes();
  }
}

case class Period(period: Int){
  def changePeriod(newPeriod: Int) = this.copy(period = newPeriod)
}

object EventData{
  import scala.collection.mutable
  import scala.collection.JavaConversions._

  val groups = mutable.Map.empty[String, EventGroup]
  var cols = new Array[String](1)
  val period = 24

  def eventGroups():Option[mutable.Map[String, EventGroup]] = {
    if(groups.size == 0){
      return None
    }
    return Some(groups)
  }

  def frequencyVectors():ArrayList[Array[Int]] = {
    val array = new ArrayList[Array[Int]]
    groups.foreach{ map =>
      val eventGroup = map._2
      array.add(eventGroup.freqCount)
    }
    return array
  }

  def eventGroupNames():Array[String] = {
    var array:Array[String] = Array.empty
    groups.foreach { map =>
       val eventgroup:EventGroup = map._2
       array = array :+ eventgroup.eventList(0).action
    }
    return array
  }

  def makeEventGroups(col_event:Int, ncols: Array[String], loaded:ArrayList[String]){
    groups.clear()
    cols = ncols

    loaded.foreach{ row =>
      val record = row split ","
      val action = record(col_event)
      val time = record(2).replaceAll("T"," ")

      val other = record//record.filter{(s:String)=>{s!=action && s!=time}}
      val event = new Event(time,action,other)

      if(groups.contains(action)){
        groups(action) = groups(action).add(event)//copyを再代入
      }else{
        val newGroup = new EventGroup(Vector(event),0,0)
        groups += (action -> newGroup)
      }
    }

    calcPositionOfEventGroups()

    groups.foreach { map =>
      map._2.setFreqCount()
    }
  }

  def changeEventGroups(col_event:Int){
    val array = new ArrayList[Event]
    groups.foreach{ maps =>
      val eventGroup:EventGroup = maps._2
      eventGroup.eventList.foreach{ event =>
        array.add(event)
      }
    }

    groups.clear()

    array.foreach{ event =>
      val record = event.other
      val action = record(col_event)
      val time = event.time

      val newevent = new Event(time,action,record)

      if(groups.contains(action)){
        groups(action) = groups(action).add(newevent)//copyを再代入
      }else{
        val newGroup = new EventGroup(Vector(newevent),0,0)
        groups += (action -> newGroup)
      }
    }

    calcPositionOfEventGroups()

    groups.foreach { map =>
      map._2.setFreqCount()
    }
  }

  def calcPositionOfEventGroups() {
    groups.foreach{ maps =>
      val action:String = maps._1
      val eventGroup:EventGroup = maps._2

      val newPosition:(Float,Float) = eventGroup.calcPoint()
      groups(action) = eventGroup.positionChange(newPosition._1,newPosition._2)
    }
  }

 }