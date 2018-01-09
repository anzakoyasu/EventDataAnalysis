import java.util.Date
import java.text.SimpleDateFormat

object DateFormat {
  val dateformats = Array("MM/dd/yyyy hh:mm:ss a","yyyy-MM-dd HH:mm:ss","EEE MMM dd HH:mm:ss  yyyy","yyyy MM dd  HH:mm:ss","yyyy/MM/dd HH:mm")

  def dateformatOption(f:SimpleDateFormat,s:String):Option[Date] = try{Some(f.parse(s))}catch{case _ =>None}

  def getDateFormat(s:String):SimpleDateFormat = {
    if(dateformatOption(new SimpleDateFormat(dateformats(0), java.util.Locale.ENGLISH),s)!=None){
      new SimpleDateFormat(dateformats(0), java.util.Locale.ENGLISH)
    }else if(dateformatOption(new SimpleDateFormat(dateformats(1)),s)!=None){
      new SimpleDateFormat(dateformats(1))
    }else if(dateformatOption(new SimpleDateFormat(dateformats(2)),s)!=None){
      new SimpleDateFormat(dateformats(2))
    }else if(dateformatOption(new SimpleDateFormat(dateformats(3)),s)!=None){
      new SimpleDateFormat(dateformats(3))
    }else if(dateformatOption(new SimpleDateFormat(dateformats(4)),s)!=None){
      new SimpleDateFormat(dateformats(4))
    }else{
      new SimpleDateFormat(dateformats(1))
    }
  }
}