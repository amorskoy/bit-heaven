package archived.initial

object SimpleSearchLinear {
  def main(args: Array[String]): Unit = {
    val szLine = scala.io.StdIn.readLine()
    val dataLine = scala.io.StdIn.readLine()
    val elLine = scala.io.StdIn.readLine()

    val sz = szLine.toLong
    val el = elLine

    val idx = dataLine.split(' ').indexOf(el)
    println(idx)
  }
}
