package project.problem
import project.graph.{Edge, Node}
import project.solution.{SolutionUnderConstruction, TspState}

import scala.collection.mutable

class Mtsp(nodes: List[Node], val matrices: List[Map[Edge, Double]])
    extends BaseProblem[TspState](
      nodes,
      edges = matrices.head.keys.toList,
      startingNode = nodes.head, //TODO instead this should probably be the first city in the input file  but for TSP it's irrelevant
      matrices.length
    ) {
  
  private val allNodes = nodes.toSet
  assert(allNodes.size == nodes.size)
  override def evaluate(solution: SolutionUnderConstruction[TspState]): List[Double] = {
    (solution.nodes :+ solution.nodes.head)
      .sliding(2)
      .map(pair => Edge(pair.head, pair.last))
      .map(edge => matrices.map(matrix => matrix(edge)))
      .toList
      .transpose
      .map(_.sum)

  }

  override def getPossibleMoves(solution: SolutionUnderConstruction[TspState]): collection.Set[Node] = {
    solution.state.nodesToVisit
  }

  override def initState: TspState = TspState((allNodes - startingNode).to(mutable.Set))

  override protected def updateState(state: TspState, node: Node): TspState = {
    state.nodesToVisit -= node
    state
  }

  override def getHeuristicValue(edge: Edge): List[Double] =
    matrices.map(matrix => 1.0 / matrix(edge))
}
