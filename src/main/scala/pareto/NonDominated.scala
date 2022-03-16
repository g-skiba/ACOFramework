package pareto

/** Function that finds pareto front for minimization of criteria
  */
def getParetoFrontMin(data: List[List[Double]]): List[Boolean] = {
  val isEfficient = data.map(x => true).toArray
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
  isEfficient.toList
}

object NonDominated {
  def main(args: Array[String]): Unit = {
    println(getParetoFrontMin(List(List(1.0), List(2.0))) == List(true, false))
    println(getParetoFrontMin(List(List(2.0), List(1.0))) == List(false, true))
    println(getParetoFrontMin(List(List(1.0), List(1.0), List(2.0))) == List(true, true, false))
    println(getParetoFrontMin(List(List(1.0), List(1.0))) == List(true, true))
    println()

    println(getParetoFrontMin(List(List(1.0, 0.0), List(2.0, 0.0))) == List(true, false))
    println(getParetoFrontMin(List(List(2.0, 0.0), List(1.0, 0.0))) == List(false, true))
    println(getParetoFrontMin(List(List(1.0, 0.0), List(1.0, 0.0), List(2.0, 0.0))) == List(true, true, false))
    println(getParetoFrontMin(List(List(1.0, 0.0), List(1.0, 0.0))) == List(true, true))
    println()

    println(getParetoFrontMin(List(List(0.0, 1.0), List(0.0, 2.0))) == List(true, false))
    println(getParetoFrontMin(List(List(0.0, 2.0), List(0.0, 1.0))) == List(false, true))
    println(getParetoFrontMin(List(List(0.0, 1.0), List(0.0, 1.0), List(0.0, 2.0))) == List(true, true, false))
    println(getParetoFrontMin(List(List(0.0, 1.0), List(0.0, 1.0))) == List(true, true))
    println()

    println(getParetoFrontMin(List(List(1.0, 0.0), List(0.0, 2.0))) == List(true, true))
    println(getParetoFrontMin(List(List(2.0, 0.0), List(0.0, 1.0))) == List(true, true))
  }
}