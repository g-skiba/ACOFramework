package project.algorithm

import project.colony.BaseColony
import project.config.AlgorithmConfig
import project.logging.AcoLogger
import project.problem.BaseProblem
import project.repo.BaseSolutionRepo

import java.io.PrintWriter
import scala.util.Random

abstract class BaseAlgorithm {
  val problem: BaseProblem[_]
  def run(resultsWriter: AcoLogger): BaseSolutionRepo
  
  def random(seed: Option[Long]): Random = seed.map(Random(_)).getOrElse(Random())
}
