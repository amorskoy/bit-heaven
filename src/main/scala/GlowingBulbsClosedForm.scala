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
    if(primes.size == 1){
      println(primes(0) * primeNum)
      return
    }

    val pairs = for(i<-primes; j<-primes.filter(_>i)) yield (i,j)

    // Result's upper bound
    var uBound = primes(0) * primeNum

    var comMuls = pairs.map{case (i,j) => (j,uBound/(i*j))}
    var comCnt = comMuls.map(_._2).reduce(_+_)

    var primeCounts = primes.map(p=> (p, uBound / p))
      .map{case (p,d) => if(p*d == uBound && p!=primes(0)) (p,d-1) else (p,d) }
      .toList  //.map{case (p,c) => (p, c-mSub(p))}.toList

    var elCount = primeCounts.map(_._2).reduce(_+_)

    while(elCount - comCnt > primeNum + 1){
      var ord = primeCounts.sortBy{case (p,c) => -p*c}
//      ord.foreach(println)
//      println("------------")
      val first = ord(0)
      val second = ord(1)

      val o2 =  List[Tuple2[Int,Long]]( (first._1, first._2-1), (second._1, second._2-1) ) ++ ord.tail.tail
      primeCounts = o2

      elCount = elCount - 2
      uBound = uBound - primes(0)
      comMuls = pairs.map{case (i,j) => (j,uBound/(i*j))}
      comCnt = comMuls.map(_._2).reduce(_+_)

//      primeCounts.foreach(println)
//      println("+++++++++++")
    }

    val subs = comMuls.groupBy(_._1).map{case (k,v) => (k, v.map(_._2).reduce(_+_))}
    val mSub = Map[Int, Long](primes(0) -> 0) ++ subs

    val ranked = primeCounts
      .map{case (p,d) => (p,d-mSub(p)) }
      .sortBy{case (p,c) => -p*c}

    val sz = ranked.map(_._2).reduce(_+_)
    val (p,c) = if(sz  == primeNum) ranked.head else ranked.tail.head

    val res = p*c

    println(res)
  }
}
