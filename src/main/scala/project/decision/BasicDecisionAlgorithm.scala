package project.decision

import project.graph.{Edge, Node}
import project.pheromone.BasePheromoneTable
import project.problem.BaseProblem
import project.solution.SolutionUnderConstruction

import scala.annotation.tailrec
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
      pheromoneWeights: Array[Double],
      heuristicWeights: Array[Double],
      solution: SolutionUnderConstruction[T]
  )(node: Node): Double = {
    def weightedProduct(values: Array[Double], weights: Array[Double]): Double = {
      require(values.length == weights.length, s"Arrays should be of the same sizes but are ${values.length} and ${weights.length}")

      @tailrec // weighted sum is faster but weighted product gives better results
      def weightedProductInd(acc: Double, ind: Int): Double = {
        if (ind < values.length) { // b.length is the same as a.length
          weightedProductInd(acc * Math.pow(values(ind), weights(ind)), ind + 1)
        } else {
          acc
        }
      }

      weightedProductInd(1.0, 0)
    }

    val edge = Edge(solution.lastNode, node)
    val pheromone = weightedProduct(pheromoneTable.getPheromone(edge), pheromoneWeights)
    val heuristic = weightedProduct(problem.getHeuristicValue(solution, node), heuristicWeights)
    Math.pow(pheromone, alpha) * Math.pow(heuristic, beta)
  }

  override def decide(
    solution: SolutionUnderConstruction[T],
    pheromoneWeights: Array[Double],
    heuristicWeights: Array[Double]
  ): Option[Node] = {
    val initializedAssessment = assessment(alpha, beta, pheromoneWeights, heuristicWeights, solution)
    val possibleMoves = problem
      .getPossibleMoves(solution)
      .toList
    if (possibleMoves.isEmpty) {
      None
    } else {
      val nodesWithCost = possibleMoves
        .map { node =>
          (node, initializedAssessment(node))
        }

      val sumOfWeights = nodesWithCost.iterator.map(_._2).sum
      val selectedRandom = random.nextDouble() * sumOfWeights

      @scala.annotation.tailrec
      def select(acc: Double, possibilities: Iterator[(Node, Double)]): Node = {
        val elem = possibilities.next()
        val newAcc = acc + elem._2
        if (selectedRandom < newAcc || !possibilities.hasNext) elem._1
        else select(newAcc, possibilities)
      }

      Some(select(0.0, nodesWithCost.iterator))
    }
  }
}
