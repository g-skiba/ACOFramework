package project.repo

import pareto.getParetoFrontMin
import project.graph.Node
import project.logging.DebugLogger.debug
import project.solution.BaseSolution

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.SortedMap as MSortedMap

class ParetoSolutionRepo(
    areSolutionsEquivalent: (Seq[Node], Seq[Node]) => Boolean,
    paretoFrontsCount: Int = 1,
    paretoHistoryLength: Int = 0
) extends BaseSolutionRepo {
  require(paretoFrontsCount > 0, "The learning archive must retain at least one Pareto front")
  require(paretoHistoryLength >= 0, "Pareto history length must be nonnegative; zero means unlimited")

  private case class ArchiveEntry(value: BaseSolution, admittedAt: Int)

  private val paretoSolutions: MSortedMap[Int, IndexedSeq[BaseSolution]] = MSortedMap.empty
  private var globalPareto: IndexedSeq[BaseSolution] = IndexedSeq.empty
  private var learningArchive: IndexedSeq[IndexedSeq[ArchiveEntry]] = IndexedSeq.empty

  /** Ordered fronts of retained learning candidates, separate from the best-ever reporting front. */
  def learningParetoFronts: IndexedSeq[IndexedSeq[BaseSolution]] =
    learningArchive.map(_.map(_.value))

  override def addSolutions(iteration: Int, newSolutions: IndexedSeq[BaseSolution]): Unit = {
    super.addSolutions(iteration, newSolutions)

    val selectedFromNew = selectParetoFront(newSolutions)
    debug(s"Added ${selectedFromNew.size} Pareto solutions (out of ${newSolutions.size} total): ${selectedFromNew.map(_.evaluation).sortBy(_.head)}")
    paretoSolutions(iteration) = selectedFromNew
    val globalWithNew = globalPareto ++ selectedFromNew
    globalPareto = selectParetoFront(globalWithNew)

    updateLearningArchive(iteration, newSolutions)
  }

  override def globalParetoSolutions: IndexedSeq[BaseSolution] = globalPareto

  override def paretoSolutionsForLastIteration: IndexedSeq[BaseSolution] = {
    val solutions = paretoSolutions.last._2
    debug(s"Retrieved ${solutions.size} Pareto solutions from last iteration - ${solutions.map(_.evaluation).sortBy(_.head)}")
    solutions
  }

  private def updateLearningArchive(iteration: Int, newSolutions: IndexedSeq[BaseSolution]): Unit = {
    // Admission age survives front changes and rediscovery. A lifetime of k includes admission through admission + k - 1.
    val survivingEntries = learningArchive.iterator.flatten.filter { entry =>
      paretoHistoryLength == 0 || iteration - entry.admittedAt < paretoHistoryLength
    }
    val newEntries = newSolutions.iterator.map(solution => ArchiveEntry(solution, iteration))
    val candidates = ArrayBuffer.empty[ArchiveEntry]
    (survivingEntries ++ newEntries).foreach { entry =>
      if (!candidates.exists(saved => areSolutionsEquivalent(saved.value.solution, entry.value.solution))) {
        candidates += entry
      }
    }

    var remaining: IndexedSeq[ArchiveEntry] = candidates.toVector
    val fronts = Vector.newBuilder[IndexedSeq[ArchiveEntry]]
    var frontCount = 0
    while (remaining.nonEmpty && frontCount < paretoFrontsCount) {
      val isFront = getParetoFrontMin(remaining)(
        _.value.evaluation,
        (s1, s2) => areSolutionsEquivalent(s1.value.solution, s2.value.solution)
      )
      val (front, rest) = remaining.zip(isFront).partition(_._2)
      fronts += front.map(_._1)
      remaining = rest.map(_._1)
      frontCount += 1
    }
    learningArchive = fronts.result()
  }

  private def selectParetoFront(solutions: IndexedSeq[BaseSolution]): IndexedSeq[BaseSolution] = {
    solutions.iterator.zip(getParetoFrontMin(solutions)(_.evaluation, (s1, s2) => areSolutionsEquivalent(s1.solution, s2.solution)).iterator).collect {
      case (v, true) => v
    }.toVector
  }
}
