package archived.normaltype

import scala.collection.parallel.ParIterable
import scala.collection.parallel.mutable.ParArray
import scala.util.control.Breaks._

object NormalTypeLinearWindowParCol {
  def main(args: Array[String]): Unit = {
    val szLine = scala.io.StdIn.readLine()
    val dataLine = scala.io.StdIn.readLine()

    val sz = szLine.toLong
    var data = dataLine.split(" ").map(_.toLong)
    val uniqs = data.distinct
    val ndv = uniqs.length

    val iData = data.zipWithIndex.par
    val gData = iData.groupBy{case (v,i) => v}.map{case (v, t) => t.map(_._2)}

    var cnt = 0L

    breakable{
      iData.foreach(v => {
        val l = v._2
        var r = l + ndv - 1

        var isWindowSubset = false
        var ranges = ParIterable[ParArray[Int]]()

        while( !isWindowSubset && r < sz){
          println(s"$l, $r")

          ranges = gData.map(a => a.filter(ai => ai>=l && ai<=r))
          isWindowSubset = ranges.find(_.isEmpty).isEmpty

          if(!isWindowSubset) r = r + 1
        }

        if(!isWindowSubset) break()

        cnt = cnt + 1
        cnt = cnt + sz - r - 1
      })
    }

    println(cnt)
  }
}
