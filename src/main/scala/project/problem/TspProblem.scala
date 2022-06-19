package project.problem
import project.graph.{Edge, Node}

class TspProblem(nodes: List[Node], matrix: Map[Edge, Double])
    extends BaseProblem(
      nodes,
      edges = matrix.keys.toList,
      1,
      Seq(matrix)
    ) {
  private val allNodes = nodes.toSet
  assert(allNodes.size == nodes.size)

  override def evaluate(solution: Seq[Node]): IndexedSeq[Double] = {
    val evaluation = (solution :+ solution.head)
      .iterator
      .sliding(2)
      .map(pair => Edge(pair.head, pair.last))
      .map(edge => arrayMatrices.head(edge.node1.number)(edge.node2.number))
      .sum
    Array[Double](evaluation)
  }

  override def getPossibleMoves(visitedNodes: Seq[Node]): Set[Node] = {
     allNodes.diff(visitedNodes.toSet)
  }
  
  override def getHeuristicValue(edge: Edge): Seq[Double] = {
    List(1.0 / arrayMatrices.head(edge.node1.number)(edge.node2.number))
  }

}