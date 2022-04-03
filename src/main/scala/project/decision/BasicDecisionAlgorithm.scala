package project.decision

import project.graph.{Edge, Node}
import project.pheromone.BasePheromoneTable
import project.problem.BaseProblem
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
      pheromoneWeights: List[Double],
      distanceWeights: List[Double]
  )(edge: Edge): Double = {
    val pheromone =
      pheromoneTable.getPheromone(edge).iterator.zip(pheromoneWeights).map(_ * _).sum
    val heuristic =
      problem.getHeuristicValue(edge).iterator.zip(distanceWeights).map(_ * _).sum
    Math.pow(pheromone, alpha) * Math.pow(heuristic, beta)
  }
  override def decide(
      visitedNodes: List[Node],
      pheromoneWeights: List[Double],
      distanceWeights: List[Double]
  ): Option[Node] = {
    val initializedAssessment = assessment(alpha, beta, pheromoneWeights, distanceWeights)
    val possibleMoves = problem
      .getPossibleMoves(visitedNodes)
      .toList
    if (possibleMoves.isEmpty) {
      return None
    }
    val edgesWithCost = possibleMoves
      .map(Edge(visitedNodes.last, _))
      .map(edge => (edge, initializedAssessment(edge)))

    val sumOfWeights = edgesWithCost.map(_._2).sum
    val selectedRandom = random.nextDouble() * sumOfWeights
    var sum = 0.0
    for { (move, weight) <- edgesWithCost } {
      sum += weight
      if (selectedRandom < sum) {
        return Some(move.node2)
      }
    }
    throw RuntimeException("Program should never reach here")
  }
}
