package project.problem

import project.graph.{Edge, Node}
import project.solution.SolutionUnderConstruction

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

abstract class BaseProblem[T](
    val nodes: Seq[Node],
    val edges: Seq[Edge],
    val startingNode: Node,
    val dimensions: Int,
    matrices: Seq[Map[Edge, Double]]
) {

  /** Function to evaluate solution into list of double
    */
  def evaluate(solution: SolutionUnderConstruction[T]): IndexedSeq[Double]

  /** Function that return all possible moves to ant, based on visited nodes
    */
  def getPossibleMoves(solution: SolutionUnderConstruction[T]): collection.Set[Node]

  def initSolution: SolutionUnderConstruction[T] =
    SolutionUnderConstruction(ArrayBuffer[Node](startingNode), initState)

  protected def initState: T

  /** @note modifies mutable structures in the solution!
   */
  def updateSolution(solution: SolutionUnderConstruction[T], newNode: Node): SolutionUnderConstruction[T] = {
    SolutionUnderConstruction(solution.nodes.append(newNode), updateState(solution.state, newNode))
  }
  /** @note might modify mutable structures in the state!
   */
  protected def updateState(state: T, node: Node): T

  /** Function to evaluate distance between nodes into doubles depending from
    * problem dimension
    */
  def getHeuristicValue(edge: Edge): Seq[Double]

  protected val arrayMatrices: Seq[Array[Array[Double]]] = matrices.map { map =>
    val maxEdgeId =
      map.keysIterator.map(e => e.node1.number.max(e.node2.number)).max
    Array.tabulate(maxEdgeId + 1, maxEdgeId + 1) { case (from, to) =>
      map.getOrElse(Edge(Node(from), Node(to)), Double.NaN)
    }
  }.toArray
}
