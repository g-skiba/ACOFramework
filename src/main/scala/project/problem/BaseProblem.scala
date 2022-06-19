package project.problem

import project.graph.{Edge, Node}

abstract class BaseProblem(
    val nodes: Seq[Node],
    val edges: Seq[Edge],
    val dimensions: Int,
    matrices: Seq[Map[Edge, Double]]
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

  protected val arrayMatrices: Seq[Array[Array[Double]]] = matrices.map { map =>
    val maxEdgeId =
      map.keysIterator.map(e => e.node1.number.max(e.node2.number)).max
    Array.tabulate(maxEdgeId + 1, maxEdgeId + 1) { case (from, to) =>
      map.getOrElse(Edge(Node(from), Node(to)), Double.NaN)
    }
  }.toArray
}
