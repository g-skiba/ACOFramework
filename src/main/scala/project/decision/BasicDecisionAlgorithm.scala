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
      heuristicWeights: Array[Double]
  )(edge: Edge): Double = {
    def weightedProduct(values: Array[Double], weights: Array[Double]): Double = {
      require(values.length == weights.length, s"Arrays should be of the same sizes but are ${values.length} and ${weights.length}")

      @tailrec
      def weightedProductInd(acc: Double, ind: Int): Double = {
        if (ind < values.length) { // b.length is the same as a.length
          weightedProductInd(acc * Math.pow(values(ind), weights(ind)), ind + 1)
        } else {
          acc
        }
      }

      weightedProductInd(1.0, 0)
    }

    val pheromone = weightedProduct(pheromoneTable.getPheromone(edge), pheromoneWeights)
    val heuristic = weightedProduct(problem.getHeuristicValue(edge), heuristicWeights)
    Math.pow(pheromone, alpha) * Math.pow(heuristic, beta)
  }

  override def decide(
    solution: SolutionUnderConstruction[T],
    pheromoneWeights: Array[Double],
    heuristicWeights: Array[Double]
  ): Option[Node] = {
    val initializedAssessment = assessment(alpha, beta, pheromoneWeights, heuristicWeights)
    val possibleMoves = problem
      .getPossibleMoves(solution)
      .toList
    if (possibleMoves.isEmpty) {
      None
    } else {
      val edgesWithCost = possibleMoves
        .map { node =>
          val edge = Edge(solution.nodes.last, node)
          (edge, initializedAssessment(edge))
        }

      val sumOfWeights = edgesWithCost.iterator.map(_._2).sum
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
