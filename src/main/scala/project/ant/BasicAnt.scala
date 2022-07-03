package project.ant

import project.decision.BaseDecisionAlgorithm
import project.graph.{Edge, Node}
import project.problem.BaseProblem
import project.solution.BaseSolution

import scala.annotation.tailrec

class BasicAnt[T](
    startingNode: Node,
    problem: BaseProblem[T],
    decision: BaseDecisionAlgorithm[T],
    val pheromoneWeights: Seq[Double],
    val heuristicWeights: Seq[Double]
) extends BaseAnt[T](
      startingNode,
      problem,
      decision
    ) {
  override def run(): BaseSolution = {
    var solution = problem.initSolution

    @tailrec
    def iter(): BaseSolution = {
      val selected = decision.decide(solution, pheromoneWeights, heuristicWeights)
      selected match
        case Some(value) =>
          solution = problem.updateSolution(solution, value)
          iter()
        case None =>
          BaseSolution(solution.nodes.toSeq, problem.evaluate(solution))
    }

    iter()
  }

}
