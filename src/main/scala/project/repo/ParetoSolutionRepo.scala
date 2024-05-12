package project.repo

import pareto.getParetoFrontMin
import project.solution.BaseSolution

import scala.collection.mutable.SortedMap as MSortedMap

class ParetoSolutionRepo extends BaseSolutionRepo {
  private val paretoSolutions: MSortedMap[Int, IndexedSeq[BaseSolution]] = MSortedMap.empty
  private var globalPareto: IndexedSeq[BaseSolution] = IndexedSeq.empty

  override def addSolutions(iteration: Int, newSolutions: IndexedSeq[BaseSolution]): Unit = {
    super.addSolutions(iteration, newSolutions)

    val selectedFromNew = selectParetoFront(newSolutions)
    paretoSolutions(iteration) = selectedFromNew
    val globalWithNew = globalPareto ++ selectedFromNew
    globalPareto = selectParetoFront(globalWithNew)
  }

  override def globalParetoSolutions: IndexedSeq[BaseSolution] = globalPareto

  override def paretoSolutionsForLastIteration: IndexedSeq[BaseSolution] = paretoSolutions.last._2

  private def selectParetoFront(solutions: IndexedSeq[BaseSolution]): IndexedSeq[BaseSolution] = {
    solutions.iterator.zip(getParetoFrontMin(solutions)(_.evaluation).iterator).collect {
      case (v, true) => v
    }.toVector
  }
}
