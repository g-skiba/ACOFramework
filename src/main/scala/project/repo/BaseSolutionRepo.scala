package project.repo

import project.logging.DebugLogger.debug
import project.solution.BaseSolution

import scala.collection.mutable.SortedMap as MSortedMap

abstract class BaseSolutionRepo {
  private val allSolutions: MSortedMap[Int, IndexedSeq[BaseSolution]] = MSortedMap.empty

  def addSolutions(iteration: Int, newSolutions: IndexedSeq[BaseSolution]): Unit = {
    allSolutions.update(iteration, newSolutions)
  }
  def globalParetoSolutions: IndexedSeq[BaseSolution]

  def solutionsForLastIteration: IndexedSeq[BaseSolution] = {
    val solutions = allSolutions.last._2
    debug(s"Retrieved all ${solutions.size} last iteration solutions from repo - ${solutions.map(_.evaluation).sortBy(_.head)}")
    solutions
  }
  def paretoSolutionsForLastIteration: IndexedSeq[BaseSolution]

  override def toString: String = {
    var string = new StringBuilder
    for (key <- allSolutions.keysIterator) {
      string = string.append(s"Iteration: $key; ")
      val itSolutions = allSolutions(key)
      val bestSolutions = itSolutions.drop(1).foldLeft(itSolutions.head.evaluation) { case (best, solution) =>
        best.zip(solution.evaluation).map { case (v1, v2) => math.min(v1, v2) }
      }
      string = string.append(s"Best by target: ${bestSolutions.mkString(", ")}\n")
    }
    string.append(s"Global best solutions: ${globalParetoSolutions.map(_.evaluation).sortBy(_.head).map(_.mkString("[", ",", "]")).mkString("; ")}")
    string.result()
  }
}
