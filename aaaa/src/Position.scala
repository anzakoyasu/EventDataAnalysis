import processing.core.PApplet
import java.util.ArrayList


case class Position(x: Float, y: Float){
  def isLocatedOnCircum(): Boolean = {
    for (h <- 0 to EventData.period - 1) {
      val theta = ViewSet.getTheta(h, EventData.period);
      val cx = ViewSet.r_view * Math.cos(theta);
      val cy = ViewSet.r_view * Math.sin(theta);
      val dist_g = PApplet.dist(x, y, cx.asInstanceOf[Float], cy.asInstanceOf[Float]);
      if (dist_g < 30) return true
    }
    return false
  }
}

class MDSPos(x: Float, y:Float) extends Position(x, y)

object MDSPositions extends ArrayList[MDSPos]{}

