package project.problem
import project.graph.{Edge, Node}

class VrpProblem(val capacity: Int, val depot: Node, nodes: List[Node], val matrix: Map[Edge, Double], val demands: Map[Node, Int])
    extends BaseProblem(
      nodes,
      edges = matrix.keys.toList,
      startingNode = depot,
      1
    ) {
  var nOTrucks = 0
  val allNodes = nodes.toSet
  val sum = sumAllEdges()
  assert(allNodes.size == nodes.size)

  def getSetDemands(capacity: Int): Set[Node] = {
    var demandSet = Set[Node]()
    for ((d,g) <- demands) {
      if (demands(d) > capacity)
        demandSet += d
    }
    demandSet
  }

  def sumAllEdges(): Double = {
    var sum = 0.toDouble
    for ((edge, distance) <- matrix){
      sum = sum + distance
    }
    sum
  }

  def getCurrentCapacity(visitedNodes: List[Node]): Int = {
    var currentCapacity = capacity
    for(node <- visitedNodes){
      val currentDemand = demands(node)
      if (node != depot){
        currentCapacity = currentCapacity - currentDemand
      }
      else{
        currentCapacity = capacity
      }
    }
    currentCapacity
  }

  override def evaluate(solution: List[Node]): List[Double] = {
    var evaluation = (solution :+ solution.head)
                      .sliding(2)
                      .map(pair => Edge(pair.head, pair.last))
                      .map(edge => matrix(edge))
                      .toList
                      .sum
    evaluation = nOTrucks * 100000 + evaluation
    nOTrucks = 0
    List[Double](evaluation)
  }

  override def getPossibleMoves(visitedNodes: List[Node]): Set[Node] = {
    val availableCapacity = getCurrentCapacity(visitedNodes)
    val availableNodes = allNodes.diff(visitedNodes.toSet)
    val availableNodesCapacity = availableNodes.diff(getSetDemands(availableCapacity))

    if (availableNodes.isEmpty){
      nOTrucks = nOTrucks + 1
      availableNodes
    } else if (availableNodesCapacity.isEmpty){
      nOTrucks = nOTrucks + 1
      Set(depot)
    } else {
      availableNodesCapacity
    }

  }
  
  override def getHeuristicValue(edge: Edge): List[Double] = {
    List(1.0 / matrix(edge))
  }
}