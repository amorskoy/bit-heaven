package archived.normaltype

import scala.util.control.Breaks._

object NormalTypeLinearWindow {
  def nextRight(r:Long):Long = r + 1

  def main(args: Array[String]): Unit = {
    val szLine = scala.io.StdIn.readLine()
    val dataLine = scala.io.StdIn.readLine()

    val sz = szLine.toLong
    var data = dataLine.split(" ").map(_.toLong)
    val ndv = data.distinct.length

    val iData = data.zipWithIndex
    val gData = iData.groupBy{case (v,i) => v}.map{case (v, t) => t.map(_._2)}

    var cnt = 0L

    breakable{
      iData.foreach(v => {
        val l:Long = v._2
        var r:Long = l + ndv - 1

        var isWindowSubset = false
        var ranges = Iterable[Array[Int]]()

        while( !isWindowSubset && r < sz){

          ranges = gData.map(a => a.filter(ai => ai>=l && ai<=r))
          isWindowSubset = ranges.find(_.isEmpty).isEmpty

          if(!isWindowSubset) r = nextRight(r)
        }

        if(!isWindowSubset) break()

        println(s"$l, $r")
        cnt = cnt + 1
        cnt = cnt + sz - r - 1
      })
    }

    println(cnt)
  }
}
