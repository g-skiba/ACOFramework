package pareto

/** Function that finds pareto front for minimization of criteria
  */
def getParetoFrontMin(data: IndexedSeq[IndexedSeq[Double]]): IndexedSeq[Boolean] = {
  val isEfficient = Array.fill(data.size)(true)
  for (i <- isEfficient.indices) {
    if (isEfficient(i)) {
      for (j <- isEfficient.indices) {
        if (i != j && isEfficient(j)) {
          isEfficient(j) =
            data(j).zip(data(i)).exists(pair => pair._1 < pair._2) || data(j).zip(data(i)).forall(pair => pair._1 == pair._2)
        }
      }
    }
  }
  isEfficient
}

object NonDominated {
  def main(args: Array[String]): Unit = {
    println(getParetoFrontMin(Vector(Vector(1.0), Vector(2.0))) == Vector(true, false))
    println(getParetoFrontMin(Vector(Vector(2.0), Vector(1.0))) == Vector(false, true))
    println(getParetoFrontMin(Vector(Vector(1.0), Vector(1.0), Vector(2.0))) == Vector(true, true, false))
    println(getParetoFrontMin(Vector(Vector(1.0), Vector(1.0))) == Vector(true, true))
    println()

    println(getParetoFrontMin(Vector(Vector(1.0, 0.0), Vector(2.0, 0.0))) == Vector(true, false))
    println(getParetoFrontMin(Vector(Vector(2.0, 0.0), Vector(1.0, 0.0))) == Vector(false, true))
    println(getParetoFrontMin(Vector(Vector(1.0, 0.0), Vector(1.0, 0.0), Vector(2.0, 0.0))) == Vector(true, true, false))
    println(getParetoFrontMin(Vector(Vector(1.0, 0.0), Vector(1.0, 0.0))) == Vector(true, true))
    println()

    println(getParetoFrontMin(Vector(Vector(0.0, 1.0), Vector(0.0, 2.0))) == Vector(true, false))
    println(getParetoFrontMin(Vector(Vector(0.0, 2.0), Vector(0.0, 1.0))) == Vector(false, true))
    println(getParetoFrontMin(Vector(Vector(0.0, 1.0), Vector(0.0, 1.0), Vector(0.0, 2.0))) == Vector(true, true, false))
    println(getParetoFrontMin(Vector(Vector(0.0, 1.0), Vector(0.0, 1.0))) == Vector(true, true))
    println()

    println(getParetoFrontMin(Vector(Vector(1.0, 0.0), Vector(0.0, 2.0))) == Vector(true, true))
    println(getParetoFrontMin(Vector(Vector(2.0, 0.0), Vector(0.0, 1.0))) == Vector(true, true))
  }
}