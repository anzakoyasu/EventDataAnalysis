import java.util.{ArrayList,Collections,Comparator,List=>JList}
import java.io.File
import java.io.FileInputStream
import java.io.IOException
import org.apache.commons.io.FileUtils
import scala.util.Try
import java.util.Date
import scala.collection.JavaConversions._


object FileLoader {
  def fileOpen(file: File):(Array[String],ArrayList[String]) = {
    val ite = FileUtils.lineIterator(file,"utf-8")
    val loadedlist = new ArrayList[String]
    var cols = new Array[String](1)

    try{//heap errorの例外
      ite.zipWithIndex.foreach{ case(row , i) =>
        val s = row split ","

        if(i==0){
          cols = s
        }else{
          loadedlist.add(row)
          //val date = dateformatOption(dateformats(format_type),datestring).get
        }
      }
      ite.close
    }catch{
      case e:Exception => println("heap error")
    }

    (cols, sortByDate(loadedlist))
  }

  def sortByDate(list: JList[String]):ArrayList[String] = {
    val r = new ArrayList[String](list)
    val date = r(1) split ","
    val dateFormat = DateFormat.getDateFormat(date(2).replaceAll("T"," "))

    Collections.sort(r, new Comparator[String]{
      override def compare(o1: String, o2:String): Int = {
        val s1 = o1 split ","
        val o1time:Date = (DateFormat.dateformatOption(dateFormat, s1(2).replaceAll("T"," ")) ).get

        val s2 = o2 split ","
        val o2time:Date = (DateFormat.dateformatOption(dateFormat, s2(2).replaceAll("T"," ")) ).get
        o1time.getTime().compareTo(o2time.getTime())
      }
    })
    r
  }


}