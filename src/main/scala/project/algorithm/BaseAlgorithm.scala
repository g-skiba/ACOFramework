package project.algorithm

import project.colony.BaseColony
import project.config.AlgorithmConfig
import project.logging.AcoLogger
import project.problem.BaseProblem
import project.repo.BaseSolutionRepo

import java.io.PrintWriter

abstract class BaseAlgorithm {
  val problem: BaseProblem[_]
  def run(resultsWriter: AcoLogger): BaseSolutionRepo
}
