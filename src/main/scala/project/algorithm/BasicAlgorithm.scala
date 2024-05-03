package project.algorithm

import pareto.getParetoFrontMin
import project.colony.{BaseColony, BasicColony}
import project.config.AlgorithmConfig
import project.logging.AcoLogger
import project.pheromone.{BasicPheromoneTable, Pheromone}
import project.problem.BaseProblem
import project.repo.{BaseSolutionRepo, ParetoSolutionRepo}
import project.solution.BaseSolution
import project.weights.ColonyWeightsSelector

import java.io.PrintWriter

class BasicAlgorithm(
    val problem: BaseProblem[_],
    algorithmConfig: AlgorithmConfig,
    seed: Option[Long] = None
) extends BaseAlgorithm {
  val solutionRepo = new ParetoSolutionRepo()

  override def run(resultsWriter: AcoLogger): BaseSolutionRepo = {
    val rnd = random(seed)

    val pheromoneTable = Pheromone.create(
      algorithmConfig.pheromoneConfig,
      problem.edges,
      problem.dimensions,
      rnd
    )

    val weightsSelector = new ColonyWeightsSelector.D2.Uniform(0.0, 1.0, algorithmConfig.antsNum)

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
      val iterationParetoFront = solutionRepo.addSolutions(iteration, solutions)
      colony.pheromoneUpdate(iterationParetoFront)

      resultsWriter.iterationResult(
        iteration,
        solutionRepo.solutionsForIteration(iteration)
      )
    }
    solutionRepo
  }
}
