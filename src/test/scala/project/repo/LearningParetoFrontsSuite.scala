package project.repo

import project.graph.Node
import project.solution.{BaseSolution, MokpSolution}

class LearningParetoFrontsSuite extends munit.FunSuite {
  private val sameNodes: (BaseSolution, BaseSolution) => Boolean = (first, second) => first.isEquivalentTo(second)

  private def solution(id: Int, first: Double, second: Double): BaseSolution =
    BaseSolution(Vector(Node(id)), Vector(first, second))

  test("disable learning archives by default without candidate processing or changes to reporting fronts") {
    for (history <- Vector(0, 2)) {
      var equivalenceChecks = 0
      val countedEquivalence: (BaseSolution, BaseSolution) => Boolean = (first, second) => {
        equivalenceChecks += 1
        first.isEquivalentTo(second)
      }
      val repo = new ParetoSolutionRepo(countedEquivalence, paretoHistoryLength = history)
      val first = solution(0, 1, 4)
      val second = solution(1, 4, 1)
      val dominated = solution(2, 5, 5)
      val better = solution(3, 0, 0)

      repo.addSolutions(0, Vector(first, second, dominated))
      assertEquals(repo.solutionsForLastIteration, Vector(first, second, dominated))
      assertEquals(repo.paretoSolutionsForLastIteration, Vector(first, second))
      assertEquals(repo.globalParetoSolutions, Vector(first, second))
      repo.addSolutions(1, Vector(better))
      repo.addSolutions(10, Vector.empty)
      assert(repo.solutionsForLastIteration.isEmpty)
      assert(repo.paretoSolutionsForLastIteration.isEmpty)
      assertEquals(repo.globalParetoSolutions, Vector(better))

      // These distinct evaluations need no equivalence checks for reporting.
      // Learning candidate deduplication would perform checks even with a zero front limit.
      assertEquals(equivalenceChecks, 0)
      val error = intercept[IllegalArgumentException](repo.learningParetoFronts)
      assert(error.getMessage.contains("paretoFrontsCount > 0"))
    }
  }

  test("retain the configured fronts and reclassify survivors when better solutions arrive") {
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

  test("deduplicate equivalent solutions without refreshing their admission age") {
    val original = MokpSolution(Vector(Node(0), Node(1)), Vector(1.0, 1.0))
    val reordered = original.copy(solution = original.solution.reverse)
    val distinct = solution(2, 1, 1)
    val dominated = solution(3, 2, 2)
    val dominatedDuplicate = solution(3, 2, 2)
    val repo = new ParetoSolutionRepo(paretoFrontsCount = 3, paretoHistoryLength = 2)

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

  test("front changes preserve admission age and expiration preserves the reporting archive") {
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

  test("support one-iteration and unlimited admission lifetimes") {
    val best = solution(0, 0, 0)
    val worse = solution(1, 1, 1)
    val oneIteration = new ParetoSolutionRepo(sameNodes, paretoFrontsCount = 2, paretoHistoryLength = 1)
    oneIteration.addSolutions(0, Vector(best))
    oneIteration.addSolutions(1, Vector(best, worse))
    assert(oneIteration.learningParetoFronts == Vector(Vector(best), Vector(worse)))
    oneIteration.addSolutions(2, Vector.empty)
    assert(oneIteration.learningParetoFronts.isEmpty)
    assert(oneIteration.globalParetoSolutions == Vector(best))

    val unlimited = new ParetoSolutionRepo(sameNodes, paretoFrontsCount = 1)
    unlimited.addSolutions(0, Vector(best, worse))
    unlimited.addSolutions(100, Vector.empty)
    assert(unlimited.learningParetoFronts == Vector(Vector(best)))
    assert(unlimited.globalParetoSolutions == Vector(best))
  }

  test("expiration does not recover candidates discarded by the front limit") {
    val best = solution(0, 0, 0)
    val discarded = solution(1, 1, 1)
    val repo = new ParetoSolutionRepo(sameNodes, paretoFrontsCount = 1, paretoHistoryLength = 2)
    repo.addSolutions(0, Vector(best))
    repo.addSolutions(1, Vector(discarded))
    assert(repo.learningParetoFronts == Vector(Vector(best)))
    repo.addSolutions(2, Vector.empty)
    assert(repo.learningParetoFronts.isEmpty)
    assert(repo.globalParetoSolutions == Vector(best))
  }

  test("reject invalid front counts and history lengths") {
    def expectInvalid(create: => ParetoSolutionRepo): Unit = {
      val rejected = try {
        create
        false
      } catch {
        case _: IllegalArgumentException => true
      }
      assert(rejected)
    }

    expectInvalid(new ParetoSolutionRepo(sameNodes, paretoFrontsCount = -1))
    expectInvalid(new ParetoSolutionRepo(sameNodes, paretoHistoryLength = -1))
  }
}
