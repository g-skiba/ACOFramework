package project.problem

import mokp.MultiObjectiveKnapsackData
import pareto.Hypervolume2DCalculator
import project.graph.{Edge, Node}
import project.solution.{MokpState, SolutionUnderConstruction}

import scala.collection.mutable

// TODO reconsider ACO modeling of Knapsack problem (nodes/edges/heuristic)
class MokpProblem(data: MultiObjectiveKnapsackData)
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
  // TODO consider more capacity-aware heuristic (would require API changes)
  //  remainingUsage =
  //    sum_l(weight_l(item) / remainingCapacity_l)
  //  heuristic_k =
  //    objective_k(item) / (1.0 + remainingUsage)
  private val itemHeuristic = Array.tabulate(data.itemsCount) { item =>
    val normalizedConstraintUsage = constraintValues.indices.map { constraint =>
      if (capacities(constraint) == 0) 0.0
      else constraintValues(constraint)(item).toDouble / capacities(constraint)
    }.sum / constraintValues.length
    val burden = 1.0 + normalizedConstraintUsage
    Array(objectiveValues(0)(item) / burden, objectiveValues(1)(item) / burden).map(_.max(1.0e-9))
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
    heuristic(solution.nodes.last.number)(candidateNode.number)
  }

  override def getHypervolumeCalculator: Option[Hypervolume2DCalculator] = Some(hypervolume2DCalculator)
}

object MokpProblem {
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
