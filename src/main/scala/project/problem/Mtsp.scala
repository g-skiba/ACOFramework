package project.problem
import project.graph.{Edge, Node}
class Mtsp(nodes: Seq[Node], matrices: Seq[Map[Edge, Double]])
    extends BaseProblem(
      nodes,
      edges = matrices.head.keys.toList,
      matrices.length,
      matrices
    ) {

  private val allNodes = nodes.toSet
  assert(allNodes.size == nodes.size)

  override def evaluate(solution: Seq[Node]): IndexedSeq[Double] = {
    (solution :+ solution.head).iterator
      .sliding(2)
      .map(pair => Edge(pair.head, pair.last))
      .map(edge =>
        arrayMatrices.map(matrix =>
          matrix(edge.node1.number)(edge.node2.number)
        )
      )
      .foldLeft(Array.fill(arrayMatrices.size)(0.0)) { case (acc, cost) =>
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
    arrayMatrices.map(matrix =>
      1.0 / matrix(edge.node1.number)(edge.node2.number)
    )
}
