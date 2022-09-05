package project.problem
import project.graph.{Edge, Node}
import project.solution.{SolutionUnderConstruction, VrpState}

import scala.collection.mutable

class VrpProblem(
    val capacity: Int,
    val depot: Node,
    nodes: List[Node],
    val matrix: Map[Edge, Double],
    val demands: Map[Node, Int]
) extends BaseProblem[VrpState](
      nodes,
      edges = matrix.keys.toList,
      startingNode = depot,
      1,
      Seq(matrix)
    ) {
  private val allNodes = nodes.toSet
  private val sum = sumAllEdges()
  assert(allNodes.size == nodes.size)

  def sumAllEdges(): Double = {
    var sum = 0.toDouble
    var sum10 = 10
    for ((edge, distance) <- matrix) {
      sum = sum + distance
    }
    while {
      sum = sum / 10
      sum10 = sum10 * 10
      sum > 10
    } do ()
    sum10.toDouble
  }

  override protected def initState: VrpState = {
    val nodesToVisit = allNodes.to(mutable.Set)
    nodesToVisit -= depot
    VrpState(nodesToVisit, capacity, 1)
  }

  override protected def updateState(state: VrpState, node: Node): VrpState = {
    if (node == depot) {
      VrpState(
        state.nodesToVisit,
        capacity,
        state.vehicles + 1
      )
    } else {
      state.nodesToVisit -= node
      VrpState(
        state.nodesToVisit,
        state.remainingCapacity - demands(node),
        state.vehicles
      )
    }
  }

  override def getPossibleMoves(
      solution: SolutionUnderConstruction[VrpState]
  ): collection.Set[Node] = {
    val availableCapacity = solution.state.remainingCapacity
    val availableNodes = solution.state.nodesToVisit
    val availableNodesCapacity =
      availableNodes.filter(x => demands(x) <= availableCapacity)

    if (availableNodes.isEmpty) {
      availableNodes
    } else if (availableNodesCapacity.isEmpty) {
      Set(depot)
    } else {
      availableNodesCapacity
    }
  }

  override def evaluate(
      solution: SolutionUnderConstruction[VrpState]
  ): IndexedSeq[Double] = {
    val evaluation = (solution.nodes :+ solution.nodes.head).iterator
      .sliding(2)
      .map(pair => Edge(pair.head, pair.last))
      .map(edge => matrix(edge))
      .sum
    Vector(solution.state.vehicles * sum + evaluation)
    // TODO Vector(solution.state.vehicles + evaluation / sum)
  }

  override def getHeuristicValue(edge: Edge): Seq[Double] = {
    List(1.0 / matrix(edge))
  }
}
