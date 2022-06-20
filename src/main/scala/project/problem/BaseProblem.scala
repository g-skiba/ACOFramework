package project.problem

import project.aggregator.BasePheromoneAggregator
import project.graph.{Edge, Node}
import project.solution.SolutionUnderConstruction

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

abstract class BaseProblem[T](
    val nodes: List[Node],
    val edges: List[Edge],
    val startingNode: Node,
    val dimensions: Int
) {

  /** Function to evaluate solution into list of double
    */
  def evaluate(solution: SolutionUnderConstruction[T]): List[Double]

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
  def getHeuristicValue(edge: Edge): List[Double]
}
