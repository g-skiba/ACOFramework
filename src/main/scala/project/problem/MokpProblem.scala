package project.problem

import mokp.MultiObjectiveKnapsackData
import pareto.Hypervolume2DCalculator
import project.graph.{Edge, Node}
import project.solution.{MokpSolution, MokpState, SolutionUnderConstruction}

import scala.collection.mutable

// TODO reconsider ACO modeling of Knapsack problem (nodes/edges/heuristic)
class MokpProblem(
    data: MultiObjectiveKnapsackData,
    heuristicMode: MokpProblem.HeuristicMode = MokpProblem.HeuristicMode.DynamicRemainingCapacity
)
    extends BaseProblem[MokpState](
      nodes = ((0 until data.itemsCount).map(Node(_)) :+ Node(data.itemsCount)).toVector,
      edges = MokpProblem.edges(data.itemsCount),
      startingNode = Node(data.itemsCount),
      dimensions = 2,
      matrices = Seq.empty
    ) {
  private val itemNodes = (0 until data.itemsCount).map(Node(_)).toSet
  private val objectiveValues = data.objectives.map(_.map(_.toDouble).toArray).toArray
  private val constraintValues = data.constraints.map(_.toArray).toArray
  private val capacities = data.capacities.toArray
  private val minHeuristicValue = 1.0e-9
  private val itemObjectiveValues = Array.tabulate(data.itemsCount, dimensions) { (item, objective) =>
    objectiveValues(objective)(item)
  }
  private val itemConstraintValues = Array.tabulate(data.itemsCount, data.constraintsCount) { (item, constraint) =>
    constraintValues(constraint)(item).toDouble
  }

  private val itemHeuristic = Array.tabulate(data.itemsCount) { item =>
    val normalizedConstraintUsage = constraintValues.indices.map { constraint =>
      if (capacities(constraint) == 0) 0.0
      else constraintValues(constraint)(item).toDouble / capacities(constraint)
    }.sum / constraintValues.length
    val burden = 1.0 + normalizedConstraintUsage
    Array(objectiveValues(0)(item) / burden, objectiveValues(1)(item) / burden).map(_.max(minHeuristicValue))
  }
  private val heuristic = {
    val arr = Array.fill(maxNodePlusOne, maxNodePlusOne)(Array.empty[Double])
    edges.foreach { edge =>
      arr(edge.node1.number)(edge.node2.number) = itemHeuristic(edge.node2.number)
    }
    arr
  }
  private val hypervolume2DCalculator = {
    // MOKP maximizes profits, but the framework minimizes negative profits.
    val minReferencePoint = (
      -objectiveValues(0).sum,
      -objectiveValues(1).sum
    )
    val maxReferencePoint = (1.0, 1.0) // dominated by the empty solution (0.0, 0.0)
    new Hypervolume2DCalculator(maxReferencePoint, minReferencePoint)
  }

  override def evaluate(solution: SolutionUnderConstruction[MokpState]): IndexedSeq[Double] = {
    val selectedItems = solution.nodes.iterator.filter(_ != startingNode).map(_.number)
    val sums = selectedItems.foldLeft(Array(0.0, 0.0)) { case (acc, item) =>
      acc(0) += objectiveValues(0)(item)
      acc(1) += objectiveValues(1)(item)
      acc
    }
    Vector(-sums(0), -sums(1))
  }

  override def createSolution(nodes: Seq[Node], evaluation: IndexedSeq[Double]): MokpSolution =
    MokpSolution(nodes, evaluation)

  override def getPossibleMoves(solution: SolutionUnderConstruction[MokpState]): collection.Set[Node] = {
    solution.state.nodesToVisit.filter { node =>
      capacities.indices.forall { constraint =>
        constraintValues(constraint)(node.number) <= solution.state.remainingCapacities(constraint)
      }
    }
  }

  override protected def initState: MokpState = {
    MokpState(itemNodes.to(mutable.Set), capacities.clone())
  }

  override protected def updateState(state: MokpState, node: Node): MokpState = {
    state.nodesToVisit -= node
    capacities.indices.foreach { constraint =>
      state.remainingCapacities(constraint) -= constraintValues(constraint)(node.number)
    }
    state
  }

  override def getHeuristicValue(solution: SolutionUnderConstruction[MokpState], candidateNode: Node): Array[Double] = {
    heuristicMode match {
      case MokpProblem.HeuristicMode.StaticCapacityNormalized =>
        heuristic(solution.lastNode.number)(candidateNode.number)
      case MokpProblem.HeuristicMode.DynamicRemainingCapacity =>
        dynamicRemainingCapacityHeuristic(candidateNode.number, solution.state.remainingCapacities)
    }
  }

  private def dynamicRemainingCapacityHeuristic(item: Int, remainingCapacities: Array[Int]): Array[Double] = {
    val constraints = itemConstraintValues(item)
    var usage = 0.0
    var constraint = 0

    while (constraint < data.constraintsCount) {
      val itemUsage = constraints(constraint)
      val remainingCapacity = remainingCapacities(constraint)
      usage += {
        if (itemUsage == 0.0) 0.0
        else if (remainingCapacity <= 0) Double.PositiveInfinity
        else itemUsage / remainingCapacity
      }
      constraint += 1
    }

    val burden = 1.0 + usage / data.constraintsCount
    val objectives = itemObjectiveValues(item)
    val result = Array.ofDim[Double](dimensions)
    var objective = 0
    while (objective < dimensions) {
      result(objective) = (objectives(objective) / burden).max(minHeuristicValue)
      objective += 1
    }
    result
  }

  override def getHypervolumeCalculator: Option[Hypervolume2DCalculator] = Some(hypervolume2DCalculator)
}

object MokpProblem {
  enum HeuristicMode {
    case StaticCapacityNormalized, DynamicRemainingCapacity
  }

  private def edges(itemsCount: Int): Seq[Edge] = {
    val itemNodes = (0 until itemsCount).map(Node(_))
    val startNode = Node(itemsCount)
    for {
      from <- itemNodes :+ startNode
      to <- itemNodes
      if from != to
    } yield Edge(from, to)
  }
}
