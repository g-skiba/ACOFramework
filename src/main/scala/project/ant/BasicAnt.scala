package project.ant

import project.graph.{Edge, Node}
import project.decision.BaseDecisionAlgorithm
import project.problem.BaseProblem
import project.solution.{BaseSolution, SolutionUnderConstruction}

class BasicAnt[T](
    startingNode: Node,
    problem: BaseProblem[T],
    decision: BaseDecisionAlgorithm[T],
    val pheromoneWeights: List[Double],
    val heuristicWeights: List[Double]
) extends BaseAnt[T](
      startingNode,
      problem,
      decision
    ) {
  override def run(): BaseSolution = {
    var solution = problem.initSolution
    var dec = decision.decide(
      solution,
      pheromoneWeights,
      heuristicWeights
    )
    while (dec.isDefined) {
      solution = problem.updateSolution(solution, dec.get)
      dec = decision.decide(
        solution,
        pheromoneWeights,
        heuristicWeights
      )
    }
    BaseSolution(
      solution.nodes.toList,
      problem.evaluate(solution)
    )
  }

}
