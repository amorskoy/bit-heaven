import scala.collection.mutable
import scala.util.control.Breaks._

object NormalTypeQueue {
  def nextRight(r: Long): Long = r + 1

  def main(args: Array[String]): Unit = {
    val szLine = scala.io.StdIn.readLine()
    val dataLine = scala.io.StdIn.readLine()

    val sz = szLine.toLong
    var data = dataLine.split(" ").map(_.toLong)

    val iData = data.zipWithIndex
    val grp = iData.groupBy { case (v, i) => v }

    var cnt = 0L

    val heapMap = new mutable.HashMap[Long, mutable.Queue[Int]]()
    grp
      .map { case (v, ilst) => (v, ilst.map(_._2)) }
      .foreach { case (v, ilst) => {
        if (!heapMap.isDefinedAt(v))
          heapMap.put(v, mutable.Queue.empty[Int])

        heapMap(v).enqueue(ilst: _*)
      }
      }

    breakable{
      (0L to sz - 1) foreach (i => {
        val n = data.apply(i.toInt)
        try {
          val minMax = heapMap.map(_._2.head).max
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
