package project.decision

import project.graph.{Edge, Node}
import project.pheromone.BasePheromoneTable
import project.problem.BaseProblem

import scala.annotation.tailrec
import scala.util.Random
class BasicDecisionAlgorithm(
    alpha: Double,
    beta: Double,
    problem: BaseProblem,
    pheromoneTable: BasePheromoneTable,
    val random: Random
) extends BaseDecisionAlgorithm(problem) {

  /** calculate heuristic and pheromone value in standard way
    */
  def assessment(
      alpha: Double,
      beta: Double,
      pheromoneWeights: Seq[Double],
      heuristicWeights: Seq[Double]
  )(edge: Edge): Double = {
    @tailrec
    def sumWeighted(acc: Double, a: Iterator[Double], b: Iterator[Double]): Double = {
      if (a.hasNext && b.hasNext) {
        sumWeighted(acc + a.next() * b.next(), a, b)
      } else {
        acc
      }
    }

    val pheromone = sumWeighted(0.0, pheromoneTable.getPheromone(edge).iterator, pheromoneWeights.iterator)
    val heuristic = sumWeighted(0.0, problem.getHeuristicValue(edge).iterator, heuristicWeights.iterator)
    Math.pow(pheromone, alpha) * Math.pow(heuristic, beta)
  }

  override def decide(
      visitedNodes: Seq[Node],
      pheromoneWeights: Seq[Double],
      heuristicWeights: Seq[Double]
  ): Option[Node] = {
    val initializedAssessment = assessment(alpha, beta, pheromoneWeights, heuristicWeights)
    val possibleMoves = problem
      .getPossibleMoves(visitedNodes)
      .toList
    if (possibleMoves.isEmpty) {
      None
    } else {
      val edgesWithCost = possibleMoves
        .map(Edge(visitedNodes.last, _))
        .map(edge => (edge, initializedAssessment(edge)))

      val sumOfWeights = edgesWithCost.map(_._2).sum
      val selectedRandom = random.nextDouble() * sumOfWeights

      @scala.annotation.tailrec
      def select(acc: Double, possibilities: Iterator[(Edge, Double)]): Edge = {
        val elem = possibilities.next()
        val newAcc = acc + elem._2
        if (selectedRandom < newAcc || !possibilities.hasNext) elem._1
        else select(newAcc, possibilities)
      }

      Some(select(0.0, edgesWithCost.iterator).node2)
    }
  }
}
