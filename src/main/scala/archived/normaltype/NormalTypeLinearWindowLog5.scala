package archived.normaltype

import scala.collection.mutable
import scala.util.control.Breaks._

object NormalTypeLinearWindowLog5 {
  def nextRight(r: Long): Long = r + 1

  def main(args: Array[String]): Unit = {
    val szLine = scala.io.StdIn.readLine()
    val dataLine = scala.io.StdIn.readLine()

    val sz = szLine.toLong
    var data = dataLine.split(" ").map(_.toLong)
    val ndv = data.distinct.length

    val iData = data.zipWithIndex
    val grp = iData.groupBy { case (v, i) => v }
    val gData = grp.map { case (v, t) => t.map(_._2) }.toArray

    var cnt = 0L

    val TO_LEFT = 0
    val TO_RIGHT = 1
    val LOG_THRESH = 10

    var left = 0L

    object MinOrder extends Ordering[Int] {
      def compare(x: Int, y: Int) = y compare x
    }

    val heapMap = new mutable.HashMap[Long, mutable.PriorityQueue[Int]]()
    grp
      .map { case (v, ilst) => (v, ilst.map(_._2)) }
      .foreach { case (v, ilst) => {
        if (!heapMap.isDefinedAt(v))
          heapMap.put(v, mutable.PriorityQueue.empty(MinOrder))

        heapMap(v).enqueue(ilst: _*)
      }
      }

    breakable{
      (0L to sz - 1) foreach (i => {
        val n = data.apply(i.toInt)
        try {
          val minMax = heapMap.map(_._2.min).max
          heapMap(n).dequeue()

          cnt = cnt + 1 + sz - 1 - minMax
        }catch {
          case e:Exception => break()
        }

      })
    }


    println(cnt)
  }
}
