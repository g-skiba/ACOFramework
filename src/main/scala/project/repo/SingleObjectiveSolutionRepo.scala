package project.repo

import pareto.getParetoFrontMin
import project.solution.BaseSolution

import scala.collection.mutable.Map as MMap

class SingleObjectiveSolutionRepo extends BaseSolutionRepo {
  private var global: Option[BaseSolution] = None

  override def addSolutions(iteration: Int, newSolutions: IndexedSeq[BaseSolution]): IndexedSeq[BaseSolution] = {
    val bestFromNew = newSolutions.minBy(_.evaluation.sum)
    solutions.update(iteration, Seq(bestFromNew))

    global = global.filter(_.evaluation.sum <= bestFromNew.evaluation.sum).orElse(Some(bestFromNew))

    Array(bestFromNew)
  }

  override def globalBest: Seq[BaseSolution] = global.toSeq
}
