package project.decision

import project.graph.{Edge, Node}
import project.pheromone.BasePheromoneTable
import project.problem.BaseProblem

class BasicDecisionAlgorithm(
    problem: BaseProblem,
    pheromoneTable: BasePheromoneTable
) extends BaseDecisionAlgorithm(problem) {

  // def getPossibleNodes(
  //     visited_nodes: List[Node]
  // ): List[Node] = problem.getPossibleMoves(visited_nodes).toList

  // def assessment(
  //     edgeToPheromone: Map[(CityName, CityName), Double],
  //     edgeToDistance: Map[(CityName, CityName), Double],
  //     alpha: Double,
  //     beta: Double,
  //     notVisied: Seq[CityName]
  // )(edge: Edge): Double = {
  //   val smth = pheromoneTable.getPheromone(edge) zip problem.getDistance(
  //     edge
  //   ) zip weights
  //   val numerator: Double =
  //     Math.pow(pheromoneTable.getPheromone(edge), alpha) * Math.pow(
  //       1.0 / problem.getDistance(edge),
  //       beta
  //     )
  //   val denominator: Double = notVisied.map { l =>
  //     Math.pow(edgeToPheromone(i, l), alpha) * Math.pow(
  //       1.0 / edgeToDistance(i, l),
  //       beta
  //     )
  //   }.sum
  //   numerator / denominator
  // }

  // /** Getting probabilities based on pheromone and cost of edge
  //   */
  // def getProbabilities(
  //     current_node: Node,
  //     possible_moves: List[Node]
  // ): Map[Node, Double] = {
  //   (for {
  //     m <- possible_moves
  //   } yield {
  //     val ass: (CityName, CityName) => Double =
  //       assessment(edgeToPheromone, edgeToDistance, alpha, beta, notVisied)
  //     val mAsses: Double = ass(i, m)
  //     val sumAsses: Double = notVisied.map { n => ass(i, n) }.sum
  //     (m, mAsses / sumAsses)
  //   }).toMap

  //   ???
  // }
  def decide(
      visitedNodes: List[Node],
      pheromoneWeights: List[Double],
      distanceWeights: List[Double]
  ): Node = {
    // val possible_moves = getPossibleNodes(visited_nodes)

    ???
  }
}
