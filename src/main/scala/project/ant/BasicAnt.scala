package project.ant

import project.graph.{Edge, Node}
import project.decision.BaseDecisionAlgorithm
import project.problem.BaseProblem
import project.solution.BaseSolution
import scala.collection.mutable.ListBuffer

class BasicAnt(
    startingNode: Node,
    problem: BaseProblem,
    decision: BaseDecisionAlgorithm,
    val pheromoneWeights: List[Double],
    val heuristicWeights: List[Double]
) extends BaseAnt(
      startingNode,
      problem,
      decision
    ) {
  override def run(): BaseSolution = {
    val solution = ListBuffer[Node](startingNode)
    var dec = decision.decide(
      solution.toList,
      pheromoneWeights,
      heuristicWeights
    )
    while (dec.isDefined) {
      solution.append(dec.get)
      dec = decision.decide(
        solution.toList,
        pheromoneWeights,
        heuristicWeights
      )
    }
    BaseSolution(
      solution.toList,
      problem.evaluate(solution.toList)
    )
  }

}
