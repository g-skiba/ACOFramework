package project.algorithm

import pareto.getParetoFrontMin
import project.colony.{BaseColony, BasicColony}
import project.config.AlgorithmConfig
import project.logging.AcoLogger
import project.pheromone.{BasicPheromoneTable, Pheromone}
import project.problem.BaseProblem
import project.repo.{BaseSolutionRepo, ParetoSolutionRepo}
import project.solution.BaseSolution

import java.io.PrintWriter
import scala.util.Random

class BasicAlgorithm(
    val problem: BaseProblem[_],
    algorithmConfig: AlgorithmConfig,
    seed: Option[Long] = None
) extends BaseAlgorithm {
  val solutionRepo = new ParetoSolutionRepo()

  override def run(resultsWriter: AcoLogger): BaseSolutionRepo = {
    val heuristicWeights =
      List.fill(problem.dimensions)(1.0 / problem.dimensions)
    val pheromoneWeights =
      List.fill(problem.dimensions)(1.0 / problem.dimensions)
    val rnd = seed.map(Random(_)).getOrElse(Random())

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
      heuristicWeights,
      pheromoneWeights
    )

    for (iteration <- 0 until algorithmConfig.iterations) {
      val solutions = colony.run()
      val iterationParetoFront = solutionRepo.addSolutions(iteration, solutions)
      colony.pheromoneUpdate(iterationParetoFront)

      val minWeightedCost =
        solutions.map(_.evaluation.zip(heuristicWeights).map(_ * _).sum).min
      resultsWriter.iterationResult(
        iteration,
        solutionRepo.solutionsForIteration(iteration)
      )
    }
    solutionRepo
  }
}
