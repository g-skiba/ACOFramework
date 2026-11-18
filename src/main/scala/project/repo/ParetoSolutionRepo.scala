package project.repo

import pareto.getParetoFrontMin
import project.logging.DebugLogger.debug
import project.solution.BaseSolution

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.SortedMap as MSortedMap

class ParetoSolutionRepo(
  areSolutionsEquivalent: (BaseSolution, BaseSolution) => Boolean = (first, second) => first.isEquivalentTo(second),
  paretoFrontsCount: Int = 0,
  paretoHistoryLength: Int = 0,
  dimensions: Int = 2
) extends BaseSolutionRepo {
  require(paretoFrontsCount >= 0, "The learning archive front count must be nonnegative; zero disables the archive")
  require(paretoHistoryLength >= 0, "Pareto history length must be nonnegative; zero means unlimited")

  private case class ArchiveEntry(value: BaseSolution, admittedAt: Int)

  private val paretoSolutions: MSortedMap[Int, IndexedSeq[BaseSolution]] = MSortedMap.empty
  private var globalPareto: IndexedSeq[BaseSolution] = IndexedSeq.empty
  private var learningArchive: IndexedSeq[IndexedSeq[ArchiveEntry]] = IndexedSeq.empty
  private val buildFrontsMethod: IndexedSeq[ArchiveEntry] => IndexedSeq[IndexedSeq[ArchiveEntry]] =
    if (dimensions == 2) buildFronts2DOptimized else buildFrontsSimple
    
  /** Ordered fronts of retained learning candidates, separate from the best-ever reporting front. */
  override def learningParetoFronts: IndexedSeq[IndexedSeq[BaseSolution]] = {
    require(paretoFrontsCount > 0, "Learning Pareto fronts are disabled; ParetoFronts requires paretoFrontsCount > 0")
    learningArchive.map(_.map(_.value))
  }

  override def addSolutions(iteration: Int, newSolutions: IndexedSeq[BaseSolution]): Unit = {
    super.addSolutions(iteration, newSolutions)

    val selectedFromNew = selectParetoFront(newSolutions)
    debug(s"Added ${selectedFromNew.size} Pareto solutions (out of ${newSolutions.size} total): ${selectedFromNew.map(_.evaluation).sortBy(_.head)}")
    paretoSolutions(iteration) = selectedFromNew
    val globalWithNew = globalPareto ++ selectedFromNew
    globalPareto = selectParetoFront(globalWithNew)

    if (paretoFrontsCount > 0) updateLearningArchive(iteration, newSolutions)
  }

  override def globalParetoSolutions: IndexedSeq[BaseSolution] = globalPareto

  override def paretoSolutionsForLastIteration: IndexedSeq[BaseSolution] = {
    val solutions = paretoSolutions.last._2
    debug(s"Retrieved ${solutions.size} Pareto solutions from last iteration - ${solutions.map(_.evaluation).sortBy(_.head)}")
    solutions
  }

  private def updateLearningArchive(iteration: Int, newSolutions: IndexedSeq[BaseSolution]): Unit = {
    // Admission age survives front changes and rediscovery. A lifetime of k includes admission through admission + k - 1.
    // Survivors are already unique; copy them without comparisons.
    val candidates = ArrayBuffer.from(
      learningArchive.iterator.flatten.filter { entry =>
        paretoHistoryLength == 0 ||
          iteration - entry.admittedAt < paretoHistoryLength
      }
    )

    // Check only incoming solutions against the accepted candidate pool.
    newSolutions.foreach { solution =>
      if (!candidates.exists(saved =>
        areSolutionsEquivalent(saved.value, solution))) {
        candidates += ArchiveEntry(solution, iteration)
      }
    }

    learningArchive = buildFrontsMethod(candidates.toVector)
//    println(learningArchive.map(_.size))
  }

  private def buildFrontsSimple(
    candidates: IndexedSeq[ArchiveEntry]
  ): IndexedSeq[IndexedSeq[ArchiveEntry]] = {
    var remaining: IndexedSeq[ArchiveEntry] = candidates.toVector
    val fronts = Vector.newBuilder[IndexedSeq[ArchiveEntry]]
    var frontCount = 0
    while (remaining.nonEmpty && frontCount < paretoFrontsCount) {
      val isFront = getParetoFrontMin(remaining)(
        _.value.evaluation,
        (s1, s2) => areSolutionsEquivalent(s1.value, s2.value)
      )
      val (front, rest) = remaining.zip(isFront).partition(_._2)
      fronts += front.map(_._1)
      remaining = rest.map(_._1)
      frontCount += 1
    }
    fronts.result()
  }

  private def buildFronts2DOptimized(
    candidates: IndexedSeq[ArchiveEntry]
  ): IndexedSeq[IndexedSeq[ArchiveEntry]] = {
    require(
      dimensions == 2,
//      candidates.forall { entry =>
//        val e = entry.value.evaluation
//        e.size == 2 && !e.exists(_.isNaN)
//      },
      "Expected exactly two objectives"
    )

    // Ascending first objective, then ascending second objective.
    val sorted = candidates.sortWith { (first, second) =>
      val a = first.value.evaluation
      val b = second.value.evaluation
      a(0) < b(0) || (a(0) == b(0) && a(1) < b(1))
    }

    val fronts = ArrayBuffer.empty[ArrayBuffer[ArchiveEntry]]

    // Smallest second-objective value seen in each front.
    // These values are nondecreasing across front ranks.
    val minY = ArrayBuffer.empty[Double]

    var i = 0
    while (i < sorted.size) {
      val evaluation = sorted(i).value.evaluation
      val x = evaluation(0)
      val y = evaluation(1)

      // Distinct solutions with identical evaluations share a rank.
      var end = i + 1
      while (
        end < sorted.size &&
          sorted(end).value.evaluation(0) == x &&
          sorted(end).value.evaluation(1) == y
      ) {
        end += 1
      }

      // Find the first front that does not dominate this group.
      var low = 0
      var high = minY.size
      while (low < high) {
        val middle = low + (high - low) / 2
        if (minY(middle) <= y) low = middle + 1
        else high = middle
      }

      val rank = low
      if (rank < paretoFrontsCount) {
        if (rank == fronts.size) {
          fronts += ArrayBuffer.empty[ArchiveEntry]
          minY += y
        } else {
          minY(rank) = y
        }

        var j = i
        while (j < end) {
          fronts(rank) += sorted(j)
          j += 1
        }
      }

      i = end
    }

    fronts.iterator.map(_.toVector).toVector
  }

  private def selectParetoFront(solutions: IndexedSeq[BaseSolution]): IndexedSeq[BaseSolution] = {
    solutions.iterator.zip(getParetoFrontMin(solutions)(_.evaluation, areSolutionsEquivalent).iterator).collect {
      case (v, true) => v
    }.toVector
  }
}
