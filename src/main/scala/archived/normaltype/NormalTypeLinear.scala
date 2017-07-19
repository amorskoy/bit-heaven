package archived.normaltype

import scala.collection.mutable
import scala.util.control.Breaks._

object NormalTypeLinear {
  def main(args: Array[String]): Unit = {
    val szLine = scala.io.StdIn.readLine()
    val dataLine = scala.io.StdIn.readLine()

    var data = dataLine.split(" ").map(_.toLong)
    val ndv = data.distinct.length

    var subCount = 0
    var left = 0
    var right = left + 1

    var cnt = 0;

    while(! data.isEmpty){
      var set = new mutable.HashSet[Long]()
      var window = data.tail

      set += data.head

      breakable{
        while(!window.isEmpty){
          set += window.head
          println(s"$left, $right -> ${set.size} of $ndv => $cnt")

          if(set.size == ndv){
            cnt = cnt+1
          }else if(set.size > ndv){
            break()
          }

          window = window.tail
          right = right + 1
        }
      }

      data = data.tail
      left = left + 1
      right = left
    }

    println(cnt)
  }
}
