package pareto

/** Function that finds pareto front for minimization of criteria
  */
def getParetoFrontMin[T](data: IndexedSeq[T])(eval: T => IndexedSeq[Double]): IndexedSeq[Boolean] = {
  val isEfficient = Array.fill(data.size)(true)
  for (i <- isEfficient.indices) {
    if (isEfficient(i)) {
      for (j <- isEfficient.indices) {
        if (i != j && isEfficient(j)) {
          val scoreI = eval(data(i))
          val scoreJ = eval(data(j))
          isEfficient(j) =
            scoreJ.zip(scoreI).exists(pair => pair._1 < pair._2) || scoreJ.zip(scoreI).forall(pair => pair._1 == pair._2)
        }
      }
    }
  }
  isEfficient
}

object NonDominated {
  def main(args: Array[String]): Unit = {
    println(getParetoFrontMin(Vector(Vector(1.0), Vector(2.0)))(identity) == Vector(true, false))
    println(getParetoFrontMin(Vector(Vector(2.0), Vector(1.0)))(identity) == Vector(false, true))
    println(getParetoFrontMin(Vector(Vector(1.0), Vector(1.0), Vector(2.0)))(identity) == Vector(true, true, false))
    println(getParetoFrontMin(Vector(Vector(1.0), Vector(1.0)))(identity) == Vector(true, true))
    println()

    println(getParetoFrontMin(Vector(Vector(1.0, 0.0), Vector(2.0, 0.0)))(identity) == Vector(true, false))
    println(getParetoFrontMin(Vector(Vector(2.0, 0.0), Vector(1.0, 0.0)))(identity) == Vector(false, true))
    println(getParetoFrontMin(Vector(Vector(1.0, 0.0), Vector(1.0, 0.0), Vector(2.0, 0.0)))(identity) == Vector(true, true, false))
    println(getParetoFrontMin(Vector(Vector(1.0, 0.0), Vector(1.0, 0.0)))(identity) == Vector(true, true))
    println()

    println(getParetoFrontMin(Vector(Vector(0.0, 1.0), Vector(0.0, 2.0)))(identity) == Vector(true, false))
    println(getParetoFrontMin(Vector(Vector(0.0, 2.0), Vector(0.0, 1.0)))(identity) == Vector(false, true))
    println(getParetoFrontMin(Vector(Vector(0.0, 1.0), Vector(0.0, 1.0), Vector(0.0, 2.0)))(identity) == Vector(true, true, false))
    println(getParetoFrontMin(Vector(Vector(0.0, 1.0), Vector(0.0, 1.0)))(identity) == Vector(true, true))
    println()

    println(getParetoFrontMin(Vector(Vector(1.0, 0.0), Vector(0.0, 2.0)))(identity) == Vector(true, true))
    println(getParetoFrontMin(Vector(Vector(2.0, 0.0), Vector(0.0, 1.0)))(identity) == Vector(true, true))
  }
}