package archived.binsearch

object GlowingBulbsClosedForm {
  def main(args: Array[String]): Unit = {
    val casesNumLine = scala.io.StdIn.readLine()
    val numCases = casesNumLine.toInt

    (0 until numCases) foreach(s=>processCase())
  }

  def processCase(): Unit ={
    val sLine = scala.io.StdIn.readLine()
    val primeLine = scala.io.StdIn.readLine()

    val primes = sLine.split("").zipWithIndex.filter(_._1=="1").map(_._2+1)
    val primeNum = primeLine.toLong

    val div = primes(0).toDouble
    val rels = primes.map(div / _)

    val relSum = rels.reduce(_+_)

    val a = primeNum / relSum

    val primeCounts = rels.map(_*a).map(_.floor)

    val muls = primes.zip(primeCounts).map{case(p,c) => (p, for( i <- (1 to c.toInt)) yield p*i)}

    var mulsDist = muls
      .flatMap{case (p,v) => v.map(s=>(p,s)) }
      .groupBy(_._2).mapValues(_(0)).values
      .groupBy(_._1).map{case (p,m) => (p, m.size)}
      .toArray

    val kSum = mulsDist.map(_._2).reduce(_+_)
    if( kSum < primeNum ) mulsDist(0) = (mulsDist(0)._1, mulsDist(0)._2 + (primeNum - kSum).toInt)

    val (p, m) = mulsDist.sortBy{ case (p,c) => -p*c }.head
    val res = p * m

    println(res)
  }
}
