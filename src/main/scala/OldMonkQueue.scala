import scala.collection.mutable
import scala.util.control.Breaks._

object OldMonkQueue {
  def main(args: Array[String]): Unit = {
    val casesNumLine = scala.io.StdIn.readLine()
    val numCases = casesNumLine.toInt

    (0 until numCases) foreach(s=>processCase())
  }

  def processCase(): Unit ={
    val szLine = scala.io.StdIn.readLine()
    val aLine = scala.io.StdIn.readLine()
    val bLine = scala.io.StdIn.readLine()

    val sz = szLine.toInt
    var a = aLine.split(" ").map(_.toLong)
    var b = bLine.split(" ").map(_.toLong)

    val aIdx = mutable.LinkedHashMap[Long, Int]()
    var bIdx = mutable.LinkedHashMap[Long, Int]()

    a.zipWithIndex.foreach(s => if(!aIdx.isDefinedAt(s._1)) aIdx.put(s._1, s._2))
    b.zipWithIndex.foreach(s => bIdx.put(s._1, s._2))

    val queue = mutable.Queue.empty[Tuple2[Long, Int]]
    queue.enqueue(bIdx.toSeq: _*)

    val set = mutable.HashSet[Int]()

    breakable{
      aIdx.foreach(s => {
        val ai = s._2
        val a = s._1
        var idx = queue.head._2
        var found = false

        while(!queue.isEmpty && queue.head._1 >= a){
          idx = queue.head._2
          queue.dequeue()
          found = true
        }


        if (found) set.add(idx - ai)
        if(queue.isEmpty) break()
      })
    }

    val res = if(set.isEmpty) 0 else set.max

    println(res)
  }
}
