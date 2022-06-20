package project.problem
import project.graph.{Edge, Node}
import project.solution.{SolutionUnderConstruction, TspState}

import scala.collection.mutable

class TspProblem(nodes: List[Node], val matrix: Map[Edge, Double])
    extends BaseProblem[TspState](
      nodes,
      edges = matrix.keys.toList,
      startingNode = nodes.head, //TODO instead this should probably be the first city in the input file  but for TSP it's irrelevant
      1
    ) {
  private val allNodes = nodes.toSet
  assert(allNodes.size == nodes.size)

  override def evaluate(solution: SolutionUnderConstruction[TspState]): List[Double] = {
    val evaluation = (solution.nodes :+ solution.nodes.head)
                      .sliding(2)
                      .map(pair => Edge(pair.head, pair.last))
                      .map(edge => matrix(edge))
                      .toList
                      .sum
    List[Double](evaluation)
  }

  override def getPossibleMoves(solution: SolutionUnderConstruction[TspState]): collection.Set[Node] = {
    solution.state.nodesToVisit
  }

  override def initState: TspState = TspState((allNodes - startingNode).to(mutable.Set))

  override protected def updateState(state: TspState, node: Node): TspState = {
    state.nodesToVisit -= node
    state
  }
  
  override def getHeuristicValue(edge: Edge): List[Double] = {
    List(1.0 / matrix(edge))
  }

}