package archived.normaltype

import scala.annotation.tailrec
import scala.util.control.Breaks._

object NormalTypeLinearWindowLog2 {
  def nextRight(r: Long): Long = r + 1

  def main(args: Array[String]): Unit = {
    val szLine = scala.io.StdIn.readLine()
    val dataLine = scala.io.StdIn.readLine()

    val sz = szLine.toLong
    var data = dataLine.split(" ").map(_.toLong)
    val ndv = data.distinct.length

    val iData = data.zipWithIndex
    val gData = iData.groupBy { case (v, i) => v }.map { case (v, t) => t.map(_._2) }

    var cnt = 0L

    val TO_LEFT = 0
    val TO_RIGHT = 1
    val LOG_THRESH = ndv

    var left = 0L

    @tailrec
    def findLogWinRight(l: Long, r: Long): Long = {
//      println(s"$l, $r, $phase")

      var ranges = gData.map(a => a.filter(ai => ai >= left && ai <= r))
      val isWindowSubset = ranges.find(_.isEmpty).isEmpty

      if(r-left+1 == LOG_THRESH){
          if(isWindowSubset) r else 0
      }else if(r-left+1 < LOG_THRESH) {
        0
      }
      else{
        if(isWindowSubset){
          var nr = r - (0.5 * (r-l)).toLong
          if(nr-left+1 < LOG_THRESH) nr = left + LOG_THRESH - 1

          findLogWinRight(l, nr)
        }else{
          var nr = r + (0.5 * (r-l)).toLong

          if(nr > sz -1) nr = sz - 1

          findLogWinRight(r, nr)
        }
      }
    }

    breakable {
      iData.foreach(v => {
        val l: Long = v._2
        left = l

        var r = findLogWinRight(l, sz - 1)

        cnt = cnt + 1 + sz - 1  - r

        println(s"$l, $r -> $cnt")
      })

    }

    println(cnt)
  }
}
