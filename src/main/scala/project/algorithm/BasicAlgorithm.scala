package project.algorithm

import pareto.getParetoFrontMin
import project.colony.{BaseColony, BasicColony}
import project.config.AlgorithmConfig
import project.logging.AcoLogger
import project.logging.DebugLogger.debug
import project.pheromone.{BasicPheromoneTable, Pheromone}
import project.problem.BaseProblem
import project.repo.{BaseSolutionRepo, ParetoSolutionRepo, SingleObjectiveSolutionRepo}
import project.solution.BaseSolution
import project.weights.ColonyWeightsSelector

import java.io.PrintWriter

class BasicAlgorithm(
    val problem: BaseProblem[_],
    algorithmConfig: AlgorithmConfig,
    seed: Option[Long] = None
) extends BaseAlgorithm {
  private val weightsSelector = problem.dimensions match {
    case 1 => ColonyWeightsSelector.D1
    case 2 => new ColonyWeightsSelector.D2.Uniform(0.0, 1.0, algorithmConfig.antsNum)
    case n => throw new RuntimeException(s"No weights selector implemented for $n-dimensional problem")
  }

  override def run(logger: AcoLogger): BaseSolutionRepo = {
    val solutionRepo = problem.dimensions match {
      case 1 => new SingleObjectiveSolutionRepo
      case _ => new ParetoSolutionRepo
    }
    val rnd = random(seed)

    val pheromoneTable = Pheromone.create(
      algorithmConfig.pheromoneConfig,
      problem.edges,
      problem.dimensions,
      rnd
    )

    val colony = BasicColony(
      algorithmConfig.alpha,
      algorithmConfig.beta,
      rnd,
      algorithmConfig.antsNum,
      problem,
      pheromoneTable,
      weightsSelector
    )

    for (iteration <- 0 until algorithmConfig.iterations) {
      val solutions = colony.run(iteration)
      debug(s"Created ${solutions.size} solutions: ${solutions.map(_.evaluation).sortBy(_.head)}")
      solutionRepo.addSolutions(iteration, solutions)
      colony.pheromoneUpdate(solutionRepo)

      logger.iterationResult(
        iteration,
        solutionRepo.paretoSolutionsForLastIteration,
        solutionRepo.globalParetoSolutions
      )
    }
    solutionRepo
  }
}
