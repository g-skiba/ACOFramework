package project.algorithm

import project.colony.BaseColony
import project.problem.BaseProblem
import project.colony.BasicColony
import project.config.AlgorithmConfig
import project.solution.BaseSolution
import project.repo.BasicSolutionRepo
import project.pheromone.BasicPheromoneTable
import scala.util.Random
import project.repo.BaseSolutionRepo

class SingleObjectiveSolver(
    val problem: BaseProblem,
    algorithmConfig: AlgorithmConfig,
    fixedRandom: Boolean = false
) extends BaseAlgorithm {
  val solutionRepo = new BasicSolutionRepo()

  override def run(): BaseSolutionRepo = {
    val increment = 0.05
    val alpha = 2.0
    val beta = 3.0
    val extinction = 0.05
    val heuristicWeights = List(1.0)
    val pheromoneWeights = List(1.0)
    val takenAntsToPheromoneUpdate = 1
    val pheromone = BasicPheromoneTable(
      problem.edges,
      increment,
      extinction,
      pheromoneWeights.size
    )
    val rnd = if (fixedRandom) Random(1637) else Random()
    val colony = BasicColony(
      alpha,
      beta,
      rnd,
      algorithmConfig.antsNum,
      problem,
      pheromone,
      heuristicWeights,
      pheromoneWeights
    )
    for (iteration <- 0 until algorithmConfig.iterations) {
      val solutions: List[BaseSolution] = colony.run()

      val minCost = solutions.map(_.evaluation.sum).min // for single objective it's the same as with .zip(heuristicWeights).map(_ * _)
      println(s"$iteration,$minCost")
      
      solutionRepo.addSolutions(iteration, solutions)
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
