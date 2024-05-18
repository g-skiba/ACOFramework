package project.repo

import pareto.getParetoFrontMin
import project.logging.DebugLogger.debug
import project.solution.BaseSolution

import scala.collection.mutable.SortedMap as MSortedMap

class SingleObjectiveSolutionRepo extends BaseSolutionRepo {
  private val bestSolutions: MSortedMap[Int, BaseSolution] = MSortedMap.empty
  private var global: Option[BaseSolution] = None

  override def addSolutions(iteration: Int, newSolutions: IndexedSeq[BaseSolution]): Unit = {
    super.addSolutions(iteration, newSolutions)
    val bestFromNew = newSolutions.minBy(_.evaluation.sum)
    bestSolutions(iteration) = bestFromNew
    global = global.filter(_.evaluation.sum <= bestFromNew.evaluation.sum).orElse(Some(bestFromNew))
  }

  override def globalParetoSolutions: IndexedSeq[BaseSolution] = global.toIndexedSeq

  override def paretoSolutionsForLastIteration: IndexedSeq[BaseSolution] = {
    val solution = bestSolutions.last._2
    debug(s"Retrieved best solution from last iteration: ${solution.evaluation}")
    IndexedSeq(solution)
  }
}
