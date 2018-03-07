import java.util.ArrayList

object DTW {
  def dtwDist(a: ArrayList[Float], b: ArrayList[Float]) :Float = {
    val aLen = a.size();
    val bLen = b.size();

    val cost = Array.ofDim[Float](aLen,bLen)
    val dist = Array.ofDim[Float](aLen,bLen)
    val window = if(aLen > bLen) aLen else bLen

    cost(0)(0) = d(a.get(0), b.get(0))

    for(i <- 1 to aLen - 1){
      cost(i)(0) = d(a.get(i),b.get(0))
      dist(i)(0) = dist(i-1)(0) + cost(i)(0)
    }
    for(j <- 1 to bLen - 1){
      cost(0)(j) = d(a.get(0),b.get(j))
      dist(0)(j) = dist(0)(j-1) + cost(0)(j)
    }

    for(i <- 1 to aLen - 1){
      val winStart = if(1 > i - window) 1 else i - window
      val winEnd = if(bLen < i + window) bLen else i + window

      for(j <- winStart to winEnd-1){
        val choices = List(dist(i-1)(j), dist(i)(j-1), dist(i-1)(j-1))
        cost(i)(j) = d(a.get(i),b.get(j))
        dist(i)(j) = choices.min + cost(i)(j)
      }
    }
    return dist(aLen-1)(bLen-1)
  }

  def d(a: Float, b: Float) :Float = {
    return Math.abs(a - b);
  }
}