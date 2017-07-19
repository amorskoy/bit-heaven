package archived.initial

object TestLinear {
  def main(args: Array[String]): Unit = {
    val line1 = scala.io.StdIn.readLine()
    val line2 = scala.io.StdIn.readLine()

    val (sz, el) = line1.split(" ") match { case Array(a,b) => (a.toInt, b.toInt)}
    val idx = line2.split(' ').reverse.map(_.toInt).indexOf(el)
    val out = if(idx > -1) sz - idx  else -1

    println(out)
  }
}
