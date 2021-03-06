package project.ant

import project.decision.BaseDecisionAlgorithm
import project.graph.{Edge, Node}
import project.problem.BaseProblem
import project.solution.BaseSolution

abstract class BaseAnt[T](
    startingNode: Node,
    val problem: BaseProblem[T],
    val decision: BaseDecisionAlgorithm[T]
) {
  var currentNode: Node = startingNode

  /** Function that make decision about next move of ant change currentNode to
    * chosen one and add it to visitedNodes list
    */
  def run(): BaseSolution
}
