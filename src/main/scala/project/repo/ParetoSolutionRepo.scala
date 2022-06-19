package project.repo

import pareto.getParetoFrontMin
import project.solution.BaseSolution
import scala.collection.mutable.{Map => MMap}

class ParetoSolutionRepo extends BaseSolutionRepo {
  private var global: IndexedSeq[BaseSolution] = IndexedSeq.empty

  override def addSolutions(iteration: Int, newSolutions: IndexedSeq[BaseSolution]): IndexedSeq[BaseSolution] = {
    val selectedFromNew = selectParetoFront(newSolutions)
    solutions.update(iteration, selectedFromNew)

    val globalWithNew = global ++ selectedFromNew
    global = selectParetoFront(globalWithNew)

    selectedFromNew
  }

  override def globalBest: Seq[BaseSolution] = global

  private def selectParetoFront(solutions: IndexedSeq[BaseSolution]): IndexedSeq[BaseSolution] = {
    solutions.iterator.zip(getParetoFrontMin(solutions)(_.evaluation).iterator).collect {
      case (v, true) => v
    }.toVector
  }
}
