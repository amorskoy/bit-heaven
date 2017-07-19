package archived.normaltype

import scala.annotation.tailrec
import scala.util.control.Breaks._

object NormalTypeLinearWindowLog {
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

    @tailrec
    def findLogWinRight(l: Long, r: Long, rold:Long, phase: Int = TO_LEFT): Long = {
      var ranges = Iterable[Array[Int]]()
//      println(s"$l, $r, $phase")

      ranges = gData.map(a => a.filter(ai => ai >= l && ai <= r))
      var isWindowSubset = ranges.find(_.isEmpty).isEmpty

      if (isWindowSubset) {
        if (phase == TO_LEFT) {
          val med = (0.5*(r+l)).toLong
          val nr = if (r - med < LOG_THRESH) r - 1 else med

          if( r-l+1 == ndv ) r
          else findLogWinRight(l, nr, r, TO_LEFT)
        } else{
          val brd = (0.5*(rold+r)).toLong

          if (rold-r == 1) r
          else if (rold-r < LOG_THRESH) r  //inverse
          else{
            val med = (0.5*(r+l)).toLong
            findLogWinRight(l, med, r, TO_LEFT)
          }
        }
      } else {
        val med = (0.5*(r+rold)).toLong

        if(r-l+1 <= ndv && r == sz-1) r
        else if(rold-r==1) rold
        else if (med-r < LOG_THRESH) findLogWinRight(l, r+1, rold, TO_RIGHT)
        else findLogWinRight(l, med, rold, TO_RIGHT)
      }
    }

    breakable {
      iData.foreach(v => {
        val l: Long = v._2
        var r: Long = l + ndv - 1

        if( l==489 ) {
          val t = 0
        }

        r = findLogWinRight(l, sz - 1, sz - 1)

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
