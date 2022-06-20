package project.decision

import project.graph.{Edge, Node}
import project.pheromone.BasePheromoneTable
import project.problem.BaseProblem
import project.solution.SolutionUnderConstruction

import scala.util.Random
class BasicDecisionAlgorithm[T](
    alpha: Double,
    beta: Double,
    problem: BaseProblem[T],
    pheromoneTable: BasePheromoneTable,
    val random: Random
) extends BaseDecisionAlgorithm[T](problem) {

  /** calculate heuristic and pheromone value in standard way
    */
  def assessment(
      alpha: Double,
      beta: Double,
      pheromoneWeights: List[Double],
      heuristicWeights: List[Double]
  )(edge: Edge): Double = {
    val pheromone =
      pheromoneTable.getPheromone(edge).iterator.zip(pheromoneWeights).map(_ * _).sum
    val heuristic =
      problem.getHeuristicValue(edge).iterator.zip(heuristicWeights).map(_ * _).sum
    Math.pow(pheromone, alpha) * Math.pow(heuristic, beta)
  }
  override def decide(
    solution: SolutionUnderConstruction[T],
    pheromoneWeights: List[Double],
    heuristicWeights: List[Double]
  ): Option[Node] = {
    val initializedAssessment = assessment(alpha, beta, pheromoneWeights, heuristicWeights)
    val possibleMoves = problem
      .getPossibleMoves(solution)
      .toList
    if (possibleMoves.isEmpty) {
      return None
    }
    val edgesWithCost = possibleMoves
      .map(Edge(solution.nodes.last, _))
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
