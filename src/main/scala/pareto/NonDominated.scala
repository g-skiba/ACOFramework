package pareto

import project.graph.Node
import project.solution.BaseSolution

/** Function that finds pareto front for minimization of criteria
  */
def getParetoFrontMin[T](data: IndexedSeq[T])(eval: T => IndexedSeq[Double]): IndexedSeq[Boolean] = {
  val isEfficient = Array.fill(data.size)(true)
  for (i <- isEfficient.indices) {
    if (isEfficient(i)) {
      for (j <- isEfficient.indices) {
        if (i != j && isEfficient(j)) {
          val dataI = data(i)
          val dataJ = data(j)

          val scoreI = eval(dataI)
          val scoreJ = eval(dataJ)

          def betterScore = scoreJ.zip(scoreI).exists(pair => pair._1 < pair._2)
          def sameScoreButDifferentData = scoreJ.zip(scoreI).forall(pair => pair._1 == pair._2) && dataI != dataJ

          isEfficient(j) = betterScore || sameScoreButDifferentData
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
    println(getParetoFrontMin(Vector(Vector(1.0), Vector(1.0), Vector(2.0)))(identity) == Vector(true, false, false))
    println(getParetoFrontMin(Vector(Vector(1.0), Vector(1.0)))(identity) == Vector(true, false))
    println()

    println(getParetoFrontMin(Vector(Vector(1.0, 0.0), Vector(2.0, 0.0)))(identity) == Vector(true, false))
    println(getParetoFrontMin(Vector(Vector(2.0, 0.0), Vector(1.0, 0.0)))(identity) == Vector(false, true))
    println(getParetoFrontMin(Vector(Vector(1.0, 0.0), Vector(1.0, 0.0), Vector(2.0, 0.0)))(identity) == Vector(true, false, false))
    println(getParetoFrontMin(Vector(Vector(1.0, 0.0), Vector(1.0, 0.0)))(identity) == Vector(true, false))
    println()

    println(getParetoFrontMin(Vector(Vector(0.0, 1.0), Vector(0.0, 2.0)))(identity) == Vector(true, false))
    println(getParetoFrontMin(Vector(Vector(0.0, 2.0), Vector(0.0, 1.0)))(identity) == Vector(false, true))
    println(getParetoFrontMin(Vector(Vector(0.0, 1.0), Vector(0.0, 1.0), Vector(0.0, 2.0)))(identity) == Vector(true, false, false))
    println(getParetoFrontMin(Vector(Vector(0.0, 1.0), Vector(0.0, 1.0)))(identity) == Vector(true, false))
    println()

    println(getParetoFrontMin(Vector(Vector(1.0, 0.0), Vector(0.0, 2.0)))(identity) == Vector(true, true))
    println(getParetoFrontMin(Vector(Vector(2.0, 0.0), Vector(0.0, 1.0)))(identity) == Vector(true, true))
    println()

    println(getParetoFrontMin(IndexedSeq(
      BaseSolution(Seq(Node(1), Node(2)), IndexedSeq(1.0)),
      BaseSolution(Seq(Node(1), Node(2)), IndexedSeq(2.0)),
    ))(_.evaluation) == Vector(true, false))
    println(getParetoFrontMin(IndexedSeq(
      BaseSolution(Seq(Node(1), Node(2)), IndexedSeq(1.0)),
      BaseSolution(Seq(Node(1), Node(2)), IndexedSeq(1.0)),
    ))(_.evaluation) == Vector(true, false))
    println(getParetoFrontMin(IndexedSeq(
      BaseSolution(Seq(Node(1), Node(2)), IndexedSeq(1.0)),
      BaseSolution(Seq(Node(1), Node(3)), IndexedSeq(1.0)),
    ))(_.evaluation) == Vector(true, true))
  }
}