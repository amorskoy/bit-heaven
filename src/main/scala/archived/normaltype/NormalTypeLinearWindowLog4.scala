package archived.normaltype

import scala.util.control.Breaks._

object NormalTypeLinearWindowLog4 {
  def nextRight(r: Long): Long = r + 1

  def main(args: Array[String]): Unit = {
    val szLine = scala.io.StdIn.readLine()
    val dataLine = scala.io.StdIn.readLine()

    val sz = szLine.toLong
    var data = dataLine.split(" ").map(_.toLong)
    val ndv = data.distinct.length

    val iData = data.zipWithIndex
    val gData = iData.groupBy { case (v, i) => v }.map { case (v, t) => t.map(_._2) }.toArray

    var cnt = 0L

    val TO_LEFT = 0
    val TO_RIGHT = 1
    val LOG_THRESH = 10

    var left = 0L

    def findWndRight(l: Long): Long = {
      val ranges = gData.map(a => a.filter(ai => ai >= l ))
      var isWindowSubset = ranges.find(_.isEmpty).isEmpty

      if(!isWindowSubset) -1
      else ranges.map(_.min).max
    }

    breakable{
      (0L to sz-1).foreach(i => {
        val r = findWndRight(i)

        if(r > -1) cnt = cnt + 1 + sz - 1 - r
        else break()
      })
    }

    println(cnt)
  }
}
