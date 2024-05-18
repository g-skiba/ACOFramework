package project.ant

import project.decision.BaseDecisionAlgorithm
import project.graph.{Edge, Node}
import project.logging.DebugLogger.debug
import project.problem.BaseProblem
import project.solution.BaseSolution
import project.weights.AntWeightsSelector

import scala.annotation.tailrec

class BasicAnt[T](
  ind: Int,
  startingNode: Node,
  problem: BaseProblem[T],
  decision: BaseDecisionAlgorithm[T],
  weightsSelector: AntWeightsSelector,
) extends BaseAnt[T](
      startingNode,
      problem,
      decision
    ) {

  override def run(iteration: Int): BaseSolution = {
    var solution = problem.initSolution
    val pheromoneWeights = weightsSelector.pheromoneWeightsForIteration(iteration)
    val heuristicWeights = weightsSelector.heuristicWeightsForIteration(iteration)
//    debug(s"Ant $ind using weights ${pheromoneWeights.mkString("(", ", ", ")")}, ${heuristicWeights.mkString("(", ", ", ")")}")

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
