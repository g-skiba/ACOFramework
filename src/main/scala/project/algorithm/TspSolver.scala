package project.algorithm

import project.colony.BaseColony
import project.problem.BaseProblem
import project.colony.BasicColony
import project.solution.BaseSolution
import project.repo.BasicSolutionRepo
import project.pheromone.BasicPheromoneTable
import scala.util.Random
import project.repo.BaseSolutionRepo

class TspSolver(
    antNumb: Int,
    val problem: BaseProblem,
    val iterations: Int
) extends BaseAlgorithm {
  val solutionRepo = new BasicSolutionRepo()

  override def run(): BaseSolutionRepo = {
    val increment = 0.05
    val alpha = 2.0
    val beta = 3.0
    val extinction = 0.05
    val distanceWeights = List(1.0)
    val pheromoneWeights = List(1.0)
    val takenAntsToPheromoneUpdate = 1
    val pheromone = BasicPheromoneTable(
      problem.edges,
      increment,
      extinction,
      pheromoneWeights.size
    )
    val rnd = Random(1637)
    val colony = BasicColony(
      alpha,
      beta,
      rnd,
      antNumb,
      problem,
      pheromone,
      distanceWeights,
      pheromoneWeights
    )
    for (iteration <- 0 until iterations) {
      val solutions: List[BaseSolution] = colony.run()
//      println(s"Step ${iteration}")

      println(
        solutions.map(_.evaluation.sum).min
      )
      solutionRepo.addSolutions(iteration, solutions)
      colony.pheromoneUpdate(
        solutions
          .sortBy(_.evaluation.sum)
          .take(takenAntsToPheromoneUpdate)
      )
    }
    println(solutionRepo)
    solutionRepo
  }
}
