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
  private val vehiclesEvalMultiplier = {
    val fromDepot = nodes.iterator.filter(_ != depot).map(n => matrix(Edge(depot, n))).sum * 2 //assume each node in separate vehicle
    val otherMaxes = nodes.iterator.filter(_ != depot) //for other nodes
      .map(n1 => nodes.iterator.filter(_ != depot).filter(_ != n1).map(n2 => matrix(Edge(n1, n2))).max) //find max edge not to depot
      .sum
    var sum = fromDepot + otherMaxes
    var tenExp = 10
    while {
      sum = sum / 10
      tenExp = tenExp * 10
      sum > 10
    } do ()
    tenExp.toDouble
  }
  assert(allNodes.size == nodes.size)
  private val heuristic = {
    val arr = Array.fill(maxNodePlusOne, maxNodePlusOne)(Array.empty[Double])
    edges.foreach { e =>
      arr(e.node1.number)(e.node2.number) = Array(1.0 / matrix(e))
    }
    arr
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
    Vector(solution.state.vehicles * vehiclesEvalMultiplier + evaluation)
    // TODO Vector(solution.state.vehicles + evaluation / vehiclesEvalMultiplier)
  }

  override def getHeuristicValue(edge: Edge): Array[Double] = {
    heuristic(edge.node1.number)(edge.node2.number)
  }
}
