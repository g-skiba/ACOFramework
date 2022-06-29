package project.algorithm

import pareto.getParetoFrontMin
import project.colony.{BaseColony, BasicColony}
import project.config.AlgorithmConfig
import project.pheromone.{BasicPheromoneTable, Pheromone}
import project.problem.BaseProblem
import project.repo.{BaseSolutionRepo, ParetoSolutionRepo}
import project.solution.BaseSolution

import java.io.PrintWriter
import scala.util.Random

class BasicAlgorithm(
    val problem: BaseProblem,
    algorithmConfig: AlgorithmConfig,
    fixedRandom: Boolean = false
) extends BaseAlgorithm {
  val solutionRepo = new ParetoSolutionRepo()

  override def run(resultsWriter: PrintWriter): BaseSolutionRepo = {
    val heuristicWeights =
      List.fill(problem.dimensions)(1.0 / problem.dimensions)
    val pheromoneWeights =
      List.fill(problem.dimensions)(1.0 / problem.dimensions)
    val pheromoneTable = Pheromone.create(
      algorithmConfig.pheromoneConfig,
      problem.edges,
      pheromoneWeights.size
    )
    val rnd = if (fixedRandom) Random(1337) else Random()
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
      val minWeightedCost = solutions.map(_.evaluation.zip(heuristicWeights).map(_ * _).sum).min
      println(s"Step $iteration:$minWeightedCost")
      resultsWriter.println(s"$iteration,$minWeightedCost")
      if (iteration % 10 == 9) {
        println(solutionRepo.solutionsForIteration(iteration).map(_.evaluation))
      }
    }
    val z = solutionRepo.solutionsIterator.flatMap(_.map(_.evaluation)).toVector
    println(z.zip(getParetoFrontMin(z)(identity)).collect { case (v, true) => v })
    println(solutionRepo)
    solutionRepo
  }
}
