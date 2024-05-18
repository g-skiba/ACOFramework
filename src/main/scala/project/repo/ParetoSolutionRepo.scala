package project.repo

import pareto.getParetoFrontMin
import project.logging.DebugLogger.debug
import project.solution.BaseSolution

import scala.collection.mutable.SortedMap as MSortedMap

class ParetoSolutionRepo extends BaseSolutionRepo {
  private val paretoSolutions: MSortedMap[Int, IndexedSeq[BaseSolution]] = MSortedMap.empty
  private var globalPareto: IndexedSeq[BaseSolution] = IndexedSeq.empty

  override def addSolutions(iteration: Int, newSolutions: IndexedSeq[BaseSolution]): Unit = {
    super.addSolutions(iteration, newSolutions)

    val selectedFromNew = selectParetoFront(newSolutions)
    debug(s"Added ${selectedFromNew.size} Pareto solutions (out of ${newSolutions.size} total): ${selectedFromNew.map(_.evaluation).sortBy(_.head)}")
    paretoSolutions(iteration) = selectedFromNew
    val globalWithNew = globalPareto ++ selectedFromNew
    globalPareto = selectParetoFront(globalWithNew)
  }

  override def globalParetoSolutions: IndexedSeq[BaseSolution] = globalPareto

  override def paretoSolutionsForLastIteration: IndexedSeq[BaseSolution] = {
    val solutions = paretoSolutions.last._2
    debug(s"Retrieved ${solutions.size} pareto solutions from last iteration - ${solutions.map(_.evaluation).sortBy(_.head)}")
    solutions
  }

  private def selectParetoFront(solutions: IndexedSeq[BaseSolution]): IndexedSeq[BaseSolution] = {
    solutions.iterator.zip(getParetoFrontMin(solutions)(_.evaluation).iterator).collect {
      case (v, true) => v
    }.toVector
  }
}
