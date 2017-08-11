package archived.initial

object TestBinarySearch {
  def main(args: Array[String]): Unit = {
    val line1 = scala.io.StdIn.readLine()
    val line2 = scala.io.StdIn.readLine()
    val line3 = scala.io.StdIn.readLine()

    val sz = line1.toInt
    val data = line2.split(' ').map(_.toLong).sorted
    val qCount = line3.toInt

    def search(from: Int, to: Int, key:Long): Int = {
      val mid = (from + to) / 2

      if(data(mid) < key) search(mid+1, to, key)
      else if (data(mid) > key) search(from, mid-1, key)
      else mid
    }

    val stream = Stream.range(0, qCount)

    for (s <- stream){
      val q = scala.io.StdIn.readLine().toLong
      val f = search(0, sz-1, q) + 1

      println(f)
    }

  }
}
