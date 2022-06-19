package project.algorithm

import project.colony.{BaseColony, BasicColony}
import project.config.AlgorithmConfig
import project.pheromone.{BasicPheromoneTable, Pheromone}
import project.problem.BaseProblem
import project.repo.{BaseSolutionRepo, ParetoSolutionRepo, SingleObjectiveSolutionRepo}
import project.solution.BaseSolution

import java.io.PrintWriter
import scala.util.Random

class SingleObjectiveSolver(
    val problem: BaseProblem,
    algorithmConfig: AlgorithmConfig,
    fixedRandom: Boolean = false
) extends BaseAlgorithm {
  val solutionRepo = new SingleObjectiveSolutionRepo()

  override def run(resultsWriter: PrintWriter): BaseSolutionRepo = {
    val heuristicWeights = List(1.0)
    val pheromoneWeights = List(1.0)

    val takenAntsToPheromoneUpdate = 1
    val pheromone = Pheromone.create(
      algorithmConfig.pheromoneConfig,
      problem.edges,
      pheromoneWeights.size
    )
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
      val selectedSolution = solutionRepo.addSolutions(iteration, solutions).head

      val minCost = selectedSolution.evaluation.sum // for single objective it's the same as with .zip(heuristicWeights).map(_ * _)
      println(s"$iteration,$minCost")
      resultsWriter.println(s"$iteration,$minCost")

      colony.pheromoneUpdate(
        solutions
          .sortBy(_.evaluation.sum) // for single objective it's the same as with .zip(heuristicWeights).map(_ * _)
          .take(takenAntsToPheromoneUpdate)
      )
    }
    println(solutionRepo)
    solutionRepo
  }
}
