package project.problem
import project.graph.{Edge, Node}

class TspProblem(nodes: List[Node], val matrix: Map[Edge, Double])
    extends BaseProblem(
      nodes,
      edges = matrix.keys.toList,
      1
    ) {
  private val allNodes = nodes.toSet
  assert(allNodes.size == nodes.size)

  override def evaluate(solution: List[Node]): List[Double] = {
    val evaluation = solution
                      .sliding(2)
                      .map(pair => Edge(pair.head, pair.last))
                      .map(edge => matrix(edge))
                      .toList
                      .sum
    List[Double](evaluation)
  }

  override def getPossibleMoves(visitedNodes: List[Node]): Set[Node] = {
     allNodes.diff(visitedNodes.toSet)
  }
  
  override def getHeuristicValue(edge: Edge): List[Double] = {
    val value = matrix(edge)
    List[Double](value)
  }

}