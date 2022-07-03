package project.algorithm

import project.colony.BaseColony
import project.problem.BaseProblem
import project.repo.BaseSolutionRepo

import java.io.PrintWriter

abstract class BaseAlgorithm {
  val problem: BaseProblem[_]
  def run(resultsWriter: PrintWriter): BaseSolutionRepo
}
