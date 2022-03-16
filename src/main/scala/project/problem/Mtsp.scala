package project.problem
import project.graph.{Edge, Node}
class Mtsp(nodes: List[Node], val matrices: List[Map[Edge, Double]])
    extends BaseProblem(
      nodes,
      edges = matrices.head.keys.toList,
      matrices.length
    ) {
  private val allNodes = nodes.toSet
  assert(allNodes.size == nodes.size)
  override def evaluate(solution: List[Node]): List[Double] = {
    solution
      .sliding(2)
      .map(pair => Edge(pair.head, pair.last))
      .map(edge => matrices.map(matrix => matrix(edge)))
      .toList
      .transpose
      .map(_.sum)

  }

  override def getPossibleMoves(visitedNodes: List[Node]): Set[Node] = {
    allNodes.diff(visitedNodes.toSet)
  }

  override def getHeuristicValue(edge: Edge): List[Double] =
    matrices.map(matrix => 1.0 / matrix(edge))
}
