package project.algorithm

import pareto.getParetoFrontMin
import project.colony.{BaseColony, BasicColony}
import project.config.AlgorithmConfig
import project.pheromone.BasicPheromoneTable
import project.problem.BaseProblem
import project.repo.{BaseSolutionRepo, BasicSolutionRepo}
import project.solution.BaseSolution

import scala.util.Random

class BasicAlgorithm(
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

    val heuristicWeights =
      List.fill(problem.dimensions)(1.0 / problem.dimensions)
    val pheromoneWeights =
      List.fill(problem.dimensions)(1.0 / problem.dimensions)
    val pheromone = BasicPheromoneTable(
      problem.edges,
      increment,
      extinction,
      pheromoneWeights.size
    )
    val rnd = if (fixedRandom) Random(1337) else Random()
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
      val iterationParetoFront = solutions.zip(getParetoFrontMin(solutions.map(_.evaluation))).collect {
        case (v, true) => v
      }
      colony.pheromoneUpdate(iterationParetoFront)
      println(
        s"Step $iteration:${solutions.map(_.evaluation.zip(heuristicWeights).map(_ * _).sum).min}"
      )
      solutionRepo.addSolutions(
        iteration,
        iterationParetoFront
      )
      if (iteration % 10 == 9) {
        println(solutionRepo.solutions(iteration).map(_.evaluation))
      }
    }
    val z = solutionRepo.solutions.flatMap(_._2.map(_.evaluation)).toList
    println(z.zip(getParetoFrontMin(z)).collect { case (v, true) => v })
    solutionRepo
  }
}
