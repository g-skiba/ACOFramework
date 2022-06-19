package project.problem
import project.graph.{Edge, Node}
class Mtsp(nodes: Seq[Node], val matrices: Seq[Map[Edge, Double]])
    extends BaseProblem(
      nodes,
      edges = matrices.head.keys.toList,
      matrices.length
    ) {
  private val allNodes = nodes.toSet
  assert(allNodes.size == nodes.size)
  override def evaluate(solution: Seq[Node]): IndexedSeq[Double] = {
    (solution :+ solution.head)
      .iterator
      .sliding(2)
      .map(pair => Edge(pair.head, pair.last))
      .map(edge => matrices.map(matrix => matrix(edge)))
      .foldLeft(Array.fill(matrices.size)(0.0)) { case (acc, cost) =>
        cost.indices.foreach { i =>
          acc(i) = acc(i) + cost(i)
        }
        acc
      }

  }

  override def getPossibleMoves(visitedNodes: Seq[Node]): Set[Node] = {
    allNodes.diff(visitedNodes.toSet)
  }

  override def getHeuristicValue(edge: Edge): Seq[Double] =
    matrices.map(matrix => 1.0 / matrix(edge))
}
