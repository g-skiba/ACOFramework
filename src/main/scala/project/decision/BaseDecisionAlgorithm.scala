package project.decision

import project.graph.{Edge, Node}
import project.problem.BaseProblem
import project.solution.SolutionUnderConstruction

abstract class BaseDecisionAlgorithm[T](val problem: BaseProblem[T]) {

  /** Function that make decision for the next ant step Parameters: currentNode
    *   - node that ant is currently in visitedNodes - all visited by ant nodes
    *     in previous steps
    */
  def decide(
    solution: SolutionUnderConstruction[T],
    pheromoneWeights: Array[Double],
    heuristicWeights: Array[Double]
  ): Option[Node]

}
