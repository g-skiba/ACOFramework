package project.repo

import project.config.SolutionsSelectionStrategy
import project.config.TwoDimPheromoneConfig.{GetType, UpdateType}
import project.graph.{Edge, Node}
import project.pheromone.{BasicPheromoneTable, ItemBasicPheromoneTable, ItemTwoDimPheromone, TwoDimPheromone}
import project.solution.BaseSolution

import scala.util.Random

class GroupedSolutionsSelectionSuite extends munit.FunSuite {
  private val sameNodes: (Seq[Node], Seq[Node]) => Boolean = (first, second) => first == second

  private def solution(id: Int, first: Double, second: Double): BaseSolution =
    BaseSolution(Vector(Node(0), Node(id)), Vector(first, second))

  test("existing strategies return one group and ParetoFronts returns ordered learning fronts") {
    val old = solution(1, 1, 1)
    val tradeoff = solution(2, 0, 4)
    val dominated = solution(3, 2, 2)
    val furtherDominated = solution(4, 3, 3)
    val repo: BaseSolutionRepo = new ParetoSolutionRepo(sameNodes, paretoFrontsCount = 3)

    assert(SolutionsSelectionStrategy.ParetoFronts(repo).isEmpty)
    repo.addSolutions(0, Vector(old))
    repo.addSolutions(1, Vector(tradeoff, dominated, furtherDominated))
    assert(SolutionsSelectionStrategy.LastIterationAll(repo) == Vector(Vector(tradeoff, dominated, furtherDominated)))
    assert(SolutionsSelectionStrategy.LastIterationPareto(repo) == Vector(Vector(tradeoff, dominated)))
    assert(SolutionsSelectionStrategy.GlobalPareto(repo) == Vector(Vector(old, tradeoff)))
    assert(SolutionsSelectionStrategy.ParetoFronts(repo) ==
      Vector(Vector(old, tradeoff), Vector(dominated), Vector(furtherDominated)))
  }

  test("preserve existing single-objective selections and reject learning Pareto fronts") {
    val best = BaseSolution(Vector(Node(0), Node(1)), Vector(1.0))
    val worse = BaseSolution(Vector(Node(0), Node(2)), Vector(2.0))
    val repo: BaseSolutionRepo = new SingleObjectiveSolutionRepo
    repo.addSolutions(0, Vector(worse, best))
    assert(SolutionsSelectionStrategy.LastIterationAll(repo) == Vector(Vector(worse, best)))
    assert(SolutionsSelectionStrategy.LastIterationPareto(repo) == Vector(Vector(best)))
    assert(SolutionsSelectionStrategy.GlobalPareto(repo) == Vector(Vector(best)))

    val unsupported = try {
      SolutionsSelectionStrategy.ParetoFronts(repo)
      false
    } catch {
      case error: UnsupportedOperationException => error.getMessage.contains("single-objective")
    }
    assert(unsupported)
  }

  test("all pheromone tables apply contributor limits within each group") {
    val a = solution(1, 1, 3)
    val b = solution(2, 2, 4)
    val c = solution(3, 3, 1)
    val edges = Vector(Edge(Node(0), Node(1)), Edge(Node(0), Node(2)), Edge(Node(0), Node(3)))
    val repo = new ParetoSolutionRepo(sameNodes, paretoFrontsCount = 2)
    repo.addSolutions(0, Vector(a, b, c))
    assert(repo.learningParetoFronts == Vector(Vector(a, c), Vector(b)))

    // Every table selects within each group, so ParetoFronts includes all three solutions for both objectives.
    // LastIterationAll has one group and still selects a,b for objective 0 and c,a for objective 1.
    for (selection <- Vector(SolutionsSelectionStrategy.LastIterationAll, SolutionsSelectionStrategy.ParetoFronts)) {
      val basic = new BasicPheromoneTable(edges, 0.25, 0.1, 2, 0.0, 1.0, selection, Some(2))
      val itemBasic = new ItemBasicPheromoneTable(edges, 0.25, 0.1, 2, 0.0, 1.0, selection, Some(2))
      basic.pheromoneUpdate(repo)
      itemBasic.pheromoneUpdate(repo)
      val expected = if (selection == SolutionsSelectionStrategy.ParetoFronts) {
        Vector.fill(3)(Vector(1.25, 1.25))
      } else Vector(Vector(1.25, 1.25), Vector(1.25, 1.0), Vector(1.0, 1.25))
      for ((edge, values) <- edges.zip(expected)) {
        assert(basic.getPheromone(edge).toVector == values)
        assert(itemBasic.getPheromone(edge).toVector == values)
      }

      for (updateType <- Vector(UpdateType.PartFromIndex, UpdateType.PartFromEvaluation)) {
        val twoDim = new TwoDimPheromone(
          edges, 0.25, 0.1, 2, 0.0, 1.0, selection, Some(2), 2, GetType.ExpectedCombination, updateType, new Random(0)
        )
        val itemTwoDim = new ItemTwoDimPheromone(
          edges, 0.25, 0.1, 2, 0.0, 1.0, selection, Some(2), 2, GetType.ExpectedCombination, updateType, new Random(0)
        )
        twoDim.pheromoneUpdate(repo)
        itemTwoDim.pheromoneUpdate(repo)
        for {
          dim <- 0 until 2
          edgeIndex <- edges.indices
          bucket <- 0 until 2
        } {
          val receivesDeposit = (dim, edgeIndex, bucket) match {
            case (0, 0, 0) | (0, 1, 1) | (1, 2, 0) | (1, 0, 1) => true
            case _ => false
          }
          val edge = edges(edgeIndex)
          val tableReceivesDeposit = if (selection == SolutionsSelectionStrategy.ParetoFronts) {
            val expectedBuckets = if (updateType == UpdateType.PartFromIndex) {
              Vector(Vector(0, 0, 1), Vector(0, 1, 0))
            } else Vector(Vector(0, 1, 1), Vector(1, 1, 0))
            bucket == expectedBuckets(dim)(edgeIndex)
          } else receivesDeposit
          val expectedValue = if (tableReceivesDeposit) 1.25 else 1.0
          assert(twoDim.pheromone(dim)(edge.cantorValue)(bucket) == expectedValue)
          assert(itemTwoDim.pheromone(dim)(edge.node2.number)(bucket) == expectedValue)
        }
      }
    }
  }

  test("basic tables use all contributors when the limit is absent or nonpositive") {
    val edges = (1 to 3).map(id => Edge(Node(0), Node(id))).toVector
    val repo = new ParetoSolutionRepo(sameNodes, paretoFrontsCount = 2)
    repo.addSolutions(0, Vector(solution(1, 1, 3), solution(2, 2, 4), solution(3, 3, 1)))
    for {
      limit <- Vector(None, Some(0), Some(-1))
      selection <- Vector(SolutionsSelectionStrategy.LastIterationAll, SolutionsSelectionStrategy.ParetoFronts)
    } {
      val basic = new BasicPheromoneTable(edges, 0.25, 0.1, 2, 0.0, 1.0, selection, limit)
      val itemBasic = new ItemBasicPheromoneTable(edges, 0.25, 0.1, 2, 0.0, 1.0, selection, limit)
      basic.pheromoneUpdate(repo)
      itemBasic.pheromoneUpdate(repo)
      for (edge <- edges) {
        assert(basic.getPheromone(edge).toVector == Vector(1.25, 1.25))
        assert(itemBasic.getPheromone(edge).toVector == Vector(1.25, 1.25))
      }
    }
  }

  test("empty selections leave basic pheromone values unchanged") {
    val edge = Edge(Node(0), Node(1))
    val repo = new ParetoSolutionRepo(sameNodes)
    repo.addSolutions(0, Vector.empty)
    for (selection <- SolutionsSelectionStrategy.values) {
      val basic = new BasicPheromoneTable(Vector(edge), 0.25, 0.1, 2, 0.0, 1.0, selection, Some(1))
      val itemBasic = new ItemBasicPheromoneTable(Vector(edge), 0.25, 0.1, 2, 0.0, 1.0, selection, Some(1))
      basic.pheromoneUpdate(repo)
      itemBasic.pheromoneUpdate(repo)
      assert(basic.getPheromone(edge).toVector == Vector(1.0, 1.0))
      assert(itemBasic.getPheromone(edge).toVector == Vector(1.0, 1.0))
    }
  }
}
