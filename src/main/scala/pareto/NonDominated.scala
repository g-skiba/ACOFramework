package pareto

/** Function that finds pareto front for minimization of criteria
  */
def getParetoFrontMin(data: List[List[Double]]): List[Boolean] = {
  val isEfficient = data.map(x => true).toArray
  for (i <- isEfficient.indices) {
    if (isEfficient(i)) {
      for (j <- isEfficient.indices) {
        if (isEfficient(j)) {
          isEfficient(j) =
            data(j).zip(data(i)).exists(pair => pair._1 < pair._2)
        }
      }
      isEfficient(i) = true
    }
  }
  isEfficient.toList
}
