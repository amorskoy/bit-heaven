package archived.normaltype

import scala.annotation.tailrec
import scala.util.control.Breaks._

object NormalTypeLinearWindowLog3 {
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
    val LOG_THRESH = 10

    var left = 0L

    @tailrec
    def findLogWinRight(l: Long, r: Long): Long = {
//      println(s"$l, $r, $phase")

      val ranges = gData.map(a => a.filter(ai => ai >= left && ai <= r))
      var isWindowSubset = ranges.find(_.isEmpty).isEmpty

      if(r-l < LOG_THRESH){
          if(isWindowSubset){
            if(r-left+1 > ndv) findLogWinRight(l, r-1) else r
          }else r + 1
      }else{
        if(isWindowSubset){
          val nr = r - (0.5 * (r-l)).toLong
          findLogWinRight(l, nr)
        }else{
          val nr = r + (0.5 * (r-l)).toLong
          findLogWinRight(r, nr)
        }
      }
    }

    breakable {
      iData.foreach(v => {
        val l: Long = v._2
        left = l

        var r = findLogWinRight(l, sz - 1)

        if(r < sz - 1)  {
          cnt = cnt + 1
          cnt = cnt + sz - r - 1
        }
        else break()

        println(s"$l, $r -> $cnt")
      })

    }

    println(cnt)
  }
}
