package project.problem

import project.graph.{Edge, Node}

abstract class BaseProblem(
    val nodes: Seq[Node],
    val edges: Seq[Edge],
    val dimensions: Int
) {

  /** Function to evaluate solution into list of double
    */
  def evaluate(solution: Seq[Node]): IndexedSeq[Double]

  /** Function that return all possible moves to ant, based on visited nodes
    */
  def getPossibleMoves(visitedNodes: Seq[Node]): Set[Node]

  /** Function to evaluate distance between nodes into doubles depending from
    * problem dimension
    */
  def getHeuristicValue(edge: Edge): Seq[Double]
}
