package project.algorithm

import project.colony.{BaseColony, BasicColony}
import project.config.AlgorithmConfig
import project.config.PheromoneConfig.PheromoneType
import project.logging.AcoLogger
import project.pheromone.{BasicPheromoneTable, Pheromone}
import project.problem.BaseProblem
import project.repo.{
  BaseSolutionRepo,
  ParetoSolutionRepo,
  SingleObjectiveSolutionRepo
}
import project.solution.BaseSolution

import java.io.PrintWriter
import scala.util.Random

class SingleObjectiveSolver(
    val problem: BaseProblem[_],
    algorithmConfig: AlgorithmConfig,
    fixedRandom: Boolean = false
) extends BaseAlgorithm {
  val solutionRepo = new SingleObjectiveSolutionRepo()

  override def run(resultsWriter: AcoLogger): BaseSolutionRepo = {
    val heuristicWeights = List(1.0)
    val pheromoneWeights = List(1.0)

    val pheromone = Pheromone.create(
      algorithmConfig.pheromoneConfig,
      problem.edges,
      optimizationTargetsCount = 1
    )
    val takenAntsToPheromoneUpdate =
      algorithmConfig.pheromoneConfig.resolveTakenAntsToPheromoneUpdate
        .getOrElse(algorithmConfig.antsNum)

    val rnd = if (fixedRandom) Random(1637) else Random()
    val colony = BasicColony(
      algorithmConfig.alpha,
      algorithmConfig.beta,
      rnd,
      algorithmConfig.antsNum,
      problem,
      pheromone,
      heuristicWeights,
      pheromoneWeights
    )
    for (iteration <- 0 until algorithmConfig.iterations) {
      val solutions = colony.run()
      val selectedSolution =
        solutionRepo.addSolutions(iteration, solutions).head

      val minCost =
        selectedSolution.evaluation.sum // for single objective it's the same as with .zip(heuristicWeights).map(_ * _)
      resultsWriter.iterationResult(
        iteration,
        solutionRepo.solutionsForIteration(iteration)
      )

      colony.pheromoneUpdate(
        solutions
          .sortBy(
            _.evaluation.sum
          ) // for single objective it's the same as with .zip(heuristicWeights).map(_ * _)
          .take(takenAntsToPheromoneUpdate)
      )
    }
    solutionRepo
  }
}
