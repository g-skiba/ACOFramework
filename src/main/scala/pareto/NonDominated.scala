package pareto

/** Function that finds pareto front for minimization of criteria
  */
def getParetoFrontMin[T](data: IndexedSeq[T])(eval: T => IndexedSeq[Double], areSolutionsEqual: (T, T) => Boolean = (s1: T, s2: T) => s1 == s2): IndexedSeq[Boolean] = {
  val isEfficient = Array.fill(data.size)(true)
  for (i <- isEfficient.indices) {
    if (isEfficient(i)) {
      for (j <- isEfficient.indices) {
        if (i != j && isEfficient(j)) {
          val dataI = data(i)
          val dataJ = data(j)

          val scoreI = eval(dataI)
          val scoreJ = eval(dataJ)

          def betterScore = scoreJ.iterator.zip(scoreI).exists(pair => pair._1 < pair._2)
          def sameScoreButDifferentData = scoreJ.iterator.zip(scoreI).forall(pair => pair._1 == pair._2) && !areSolutionsEqual(dataI, dataJ)

          isEfficient(j) = betterScore || sameScoreButDifferentData
        }
      }
    }
  }
  isEfficient
}
