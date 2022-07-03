package project.problem
import project.graph.{Edge, Node}
import project.solution.{SolutionUnderConstruction, TspState}

import scala.collection.mutable

class Mtsp(nodes: Seq[Node], matrices: Seq[Map[Edge, Double]])
    extends BaseProblem[TspState](
      nodes,
      edges = matrices.head.keys.toList,
      startingNode = nodes.head, //TODO instead this should probably be the first city in the input file  but for TSP it's irrelevant
      matrices.length,
      matrices
    ) {

  private val allNodes = nodes.toSet
  assert(allNodes.size == nodes.size)
  override def evaluate(solution: SolutionUnderConstruction[TspState]): IndexedSeq[Double] = {
    (solution.nodes :+ solution.nodes.head).iterator
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

  override def getPossibleMoves(solution: SolutionUnderConstruction[TspState]): collection.Set[Node] = {
    solution.state.nodesToVisit
  }

  override def initState: TspState = TspState((allNodes - startingNode).to(mutable.Set))

  override protected def updateState(state: TspState, node: Node): TspState = {
    state.nodesToVisit -= node
    state
  }

  override def getHeuristicValue(edge: Edge): Seq[Double] =
    arrayMatrices.map(matrix =>
      1.0 / matrix(edge.node1.number)(edge.node2.number)
    )
}
