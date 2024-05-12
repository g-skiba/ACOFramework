package project.algorithm

import project.colony.{BaseColony, BasicColony}
import project.config.AlgorithmConfig
import project.config.PheromoneConfig.PheromoneType
import project.logging.AcoLogger
import project.pheromone.{BasicPheromoneTable, Pheromone}
import project.problem.BaseProblem
import project.repo.{BaseSolutionRepo, ParetoSolutionRepo, SingleObjectiveSolutionRepo}
import project.solution.BaseSolution
import project.weights.ColonyWeightsSelector

import java.io.PrintWriter

class SingleObjectiveSolver(
    val problem: BaseProblem[_],
    algorithmConfig: AlgorithmConfig,
    seed: Option[Long] = None
) extends BaseAlgorithm {
  val solutionRepo = new SingleObjectiveSolutionRepo()

  override def run(resultsWriter: AcoLogger): BaseSolutionRepo = {
    val rnd = random(seed)

    val pheromone = Pheromone.create(
      algorithmConfig.pheromoneConfig,
      problem.edges,
      optimizationTargetsCount = 1,
      rnd
    )

    val colony = BasicColony(
      algorithmConfig.alpha,
      algorithmConfig.beta,
      rnd,
      algorithmConfig.antsNum,
      problem,
      pheromone,
      ColonyWeightsSelector.D1,
    )
    for (iteration <- 0 until algorithmConfig.iterations) {
      val solutions = colony.run(iteration)
      solutionRepo.addSolutions(iteration, solutions)
      colony.pheromoneUpdate(solutionRepo)

      resultsWriter.iterationResult(
        iteration,
        solutionRepo.paretoSolutionsForLastIteration
      )
    }
    solutionRepo
  }
}
