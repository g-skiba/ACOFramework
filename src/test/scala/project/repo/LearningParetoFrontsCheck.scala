package project.repo

import project.graph.Node
import project.solution.BaseSolution

/** Run with `sbt "Test/runMain project.repo.LearningParetoFrontsCheck"`. */
object LearningParetoFrontsCheck {
  private val sameNodes: (Seq[Node], Seq[Node]) => Boolean = (first, second) => first == second

  private def solution(id: Int, first: Double, second: Double): BaseSolution =
    BaseSolution(Vector(Node(id)), Vector(first, second))

  def main(args: Array[String]): Unit = {
    retainsAndReclassifiesFronts()
    deduplicatesEquivalentSolutions()
    expiresWithoutRefreshingAdmission()
    handlesOneIterationAndUnlimitedLifetimes()
    doesNotRecoverDiscardedCandidates()
    validatesArchiveLimits()
    println("Learning Pareto front checks passed")
  }

  private def retainsAndReclassifiesFronts(): Unit = {
    val a = solution(0, 0, 4)
    val b = solution(1, 4, 0)
    val c = solution(2, 2, 2)
    val d = solution(3, 2, 4)
    val e = solution(4, 4, 2)
    val f = solution(5, 4, 4)
    val discarded = solution(6, 5, 5)
    val batch = Vector(f, d, a, discarded, b, e, c)
    val repo = new ParetoSolutionRepo(sameNodes, paretoFrontsCount = 3)

    assert(repo.learningParetoFronts.isEmpty)
    repo.addSolutions(0, batch)
    assert(repo.learningParetoFronts == Vector(Vector(a, b, c), Vector(d, e), Vector(f)))
    assert(repo.solutionsForLastIteration == batch)
    assert(repo.paretoSolutionsForLastIteration == Vector(a, b, c))
    assert(repo.globalParetoSolutions == Vector(a, b, c))

    val better = solution(7, 0, 0)
    repo.addSolutions(1, Vector(better))
    assert(repo.learningParetoFronts == Vector(Vector(better), Vector(a, b, c), Vector(d, e)))
    assert(repo.paretoSolutionsForLastIteration == Vector(better))
    assert(repo.globalParetoSolutions == Vector(better))

    repo.addSolutions(2, Vector.empty)
    assert(repo.learningParetoFronts == Vector(Vector(better), Vector(a, b, c), Vector(d, e)))
  }

  private def deduplicatesEquivalentSolutions(): Unit = {
    val sameItems: (Seq[Node], Seq[Node]) => Boolean = (first, second) => first.toSet == second.toSet
    val original = BaseSolution(Vector(Node(0), Node(1)), Vector(1.0, 1.0))
    val reordered = original.copy(solution = original.solution.reverse)
    val distinct = solution(2, 1, 1)
    val dominated = solution(3, 2, 2)
    val dominatedDuplicate = dominated.copy()
    val repo = new ParetoSolutionRepo(sameItems, paretoFrontsCount = 3, paretoHistoryLength = 2)

    repo.addSolutions(0, Vector(original, reordered, distinct, dominated, dominatedDuplicate))
    assert(repo.learningParetoFronts == Vector(Vector(original, distinct), Vector(dominated)))
    repo.addSolutions(1, Vector(reordered, dominatedDuplicate))
    assert(repo.learningParetoFronts == Vector(Vector(original, distinct), Vector(dominated)))

    // Rediscovery in iteration 1 does not extend either original admission lifetime.
    repo.addSolutions(2, Vector.empty)
    assert(repo.learningParetoFronts.isEmpty)
    assert(repo.globalParetoSolutions == Vector(original, distinct))

    repo.addSolutions(3, Vector(reordered))
    assert(repo.learningParetoFronts == Vector(Vector(reordered)))
    repo.addSolutions(4, Vector.empty)
    assert(repo.learningParetoFronts == Vector(Vector(reordered)))
    repo.addSolutions(5, Vector.empty)
    assert(repo.learningParetoFronts.isEmpty)
  }

  private def expiresWithoutRefreshingAdmission(): Unit = {
    val old = solution(0, 1, 1)
    val better = solution(1, 0, 0)
    val repo = new ParetoSolutionRepo(sameNodes, paretoFrontsCount = 2, paretoHistoryLength = 2)

    repo.addSolutions(0, Vector(old))
    repo.addSolutions(1, Vector(better))
    assert(repo.learningParetoFronts == Vector(Vector(better), Vector(old)))
    repo.addSolutions(2, Vector.empty)
    assert(repo.learningParetoFronts == Vector(Vector(better)))
    repo.addSolutions(3, Vector.empty)
    assert(repo.learningParetoFronts.isEmpty)
    assert(repo.paretoSolutionsForLastIteration.isEmpty)
    assert(repo.globalParetoSolutions == Vector(better))
  }

  private def handlesOneIterationAndUnlimitedLifetimes(): Unit = {
    val best = solution(0, 0, 0)
    val worse = solution(1, 1, 1)
    val oneIteration = new ParetoSolutionRepo(sameNodes, paretoFrontsCount = 2, paretoHistoryLength = 1)
    oneIteration.addSolutions(0, Vector(best))
    oneIteration.addSolutions(1, Vector(best, worse))
    assert(oneIteration.learningParetoFronts == Vector(Vector(best), Vector(worse)))
    oneIteration.addSolutions(2, Vector.empty)
    assert(oneIteration.learningParetoFronts.isEmpty)
    assert(oneIteration.globalParetoSolutions == Vector(best))

    val unlimited = new ParetoSolutionRepo(sameNodes)
    unlimited.addSolutions(0, Vector(best, worse))
    unlimited.addSolutions(100, Vector.empty)
    assert(unlimited.learningParetoFronts == Vector(Vector(best)))
    assert(unlimited.globalParetoSolutions == Vector(best))
  }

  private def doesNotRecoverDiscardedCandidates(): Unit = {
    val best = solution(0, 0, 0)
    val discarded = solution(1, 1, 1)
    val repo = new ParetoSolutionRepo(sameNodes, paretoHistoryLength = 2)
    repo.addSolutions(0, Vector(best))
    repo.addSolutions(1, Vector(discarded))
    assert(repo.learningParetoFronts == Vector(Vector(best)))
    repo.addSolutions(2, Vector.empty)
    assert(repo.learningParetoFronts.isEmpty)
    assert(repo.globalParetoSolutions == Vector(best))
  }

  private def validatesArchiveLimits(): Unit = {
    def expectInvalid(create: => ParetoSolutionRepo): Unit = {
      val rejected = try {
        create
        false
      } catch {
        case _: IllegalArgumentException => true
      }
      assert(rejected)
    }

    expectInvalid(new ParetoSolutionRepo(sameNodes, paretoFrontsCount = 0))
    expectInvalid(new ParetoSolutionRepo(sameNodes, paretoFrontsCount = -1))
    expectInvalid(new ParetoSolutionRepo(sameNodes, paretoHistoryLength = -1))
  }
}
