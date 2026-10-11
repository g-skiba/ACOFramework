package project.pheromone

import project.config.{PheromoneConfig, SolutionsSelectionStrategy, TwoDimPheromoneConfig}
import project.config.PheromoneConfig.PheromoneType
import project.config.TwoDimPheromoneConfig.{GetType, UpdateType}
import project.graph.{Edge, Node}
import project.repo.ParetoSolutionRepo
import project.solution.BaseSolution

import scala.util.Random

class ItemTwoDimParetoFrontUpdateSuite extends munit.FunSuite {
  private val sameNodes: (Seq[Node], Seq[Node]) => Boolean = (first, second) => first == second
  // The starting node is outside the allocated item table, so depositing on it would fail.
  private val start = Node(6)
  private val edges = (0 until 6).map(id => Edge(start, Node(id))).toVector
  private val solutions = Vector(
    solution(0, 1, 5), solution(1, 5, 1),
    solution(2, 2, 6), solution(3, 6, 2),
    solution(4, 3, 7), solution(5, 7, 3)
  )

  private def solution(id: Int, first: Double, second: Double): BaseSolution =
    BaseSolution(Vector(start, Node(id)), Vector(first, second))

  private def repository(fronts: Int = 2): ParetoSolutionRepo = {
    val repo = new ParetoSolutionRepo(sameNodes, paretoFrontsCount = fronts)
    repo.addSolutions(0, solutions)
    repo
  }

  private def table(
      mode: UpdateType,
      limit: Option[Int] = Some(1),
      buckets: Int = 4,
      selection: SolutionsSelectionStrategy = SolutionsSelectionStrategy.ParetoFronts
  ): ItemTwoDimPheromone = new ItemTwoDimPheromone(
    edges, 0.25, 0.1, 2, 0.0, 1.0, selection, limit, buckets, GetType.ExpectedCombination, mode, new Random(0)
  )

  private def assertDeposits(pheromone: ItemTwoDimPheromone, deposits: Set[(Int, Int, Int)], buckets: Int = 4): Unit = {
    for {
      dim <- 0 until 2
      item <- 0 until 6
      bucket <- 0 until buckets
    } {
      val expected = if (deposits.contains((dim, item, bucket))) 1.25 else 1.0
      assert(pheromone.pheromone(dim)(item)(bucket) == expected,
        s"Unexpected deposit for objective $dim, item $item, bucket $bucket")
    }
  }

  test("select per objective within every front and use the same contributors for all bucket modes") {
    val repo = repository()
    val frontTable = table(UpdateType.PartFromParetoFront)
    frontTable.pheromoneUpdate(repo)
    assertDeposits(frontTable, Set((0, 0, 0), (0, 2, 1), (1, 1, 0), (1, 3, 1)))

    val indexTable = table(UpdateType.PartFromIndex)
    indexTable.pheromoneUpdate(repo)
    assertDeposits(indexTable, Set((0, 0, 0), (0, 2, 2), (1, 1, 0), (1, 3, 2)))

    val evaluationTable = table(UpdateType.PartFromEvaluation)
    evaluationTable.pheromoneUpdate(repo)
    assertDeposits(evaluationTable, Set((0, 0, 0), (0, 2, 3), (1, 1, 0), (1, 3, 3)))

    frontTable.afterUpdatesAction()
    for {
      dim <- 0 until 2
      item <- 0 until 6
      bucket <- 2 until 4
    } assert(math.abs(frontTable.pheromone(dim)(item)(bucket) - 0.9) < 1e-12)
  }

  test("deposit on every selected item and skip the starting node") {
    val a = BaseSolution(Vector(start, Node(0), Node(2)), Vector(1.0, 4.0))
    val b = BaseSolution(Vector(start, Node(1), Node(2)), Vector(4.0, 1.0))
    val dominated = BaseSolution(Vector(start, Node(3), Node(4)), Vector(2.0, 5.0))
    val repo = new ParetoSolutionRepo(sameNodes, paretoFrontsCount = 2)
    repo.addSolutions(0, Vector(a, b, dominated))
    val pheromone = table(UpdateType.PartFromParetoFront)
    pheromone.pheromoneUpdate(repo)
    assertDeposits(pheromone, Set(
      (0, 0, 0), (0, 2, 0), (0, 3, 1), (0, 4, 1),
      (1, 1, 0), (1, 2, 0), (1, 3, 1), (1, 4, 1)
    ))
  }

  test("use all retained solutions when the limit is absent or nonpositive") {
    val deposits = (for {
      dim <- 0 until 2
      item <- 0 until 4
    } yield (dim, item, item / 2)).toSet
    for (limit <- Vector(None, Some(0), Some(-1))) {
      val pheromone = table(UpdateType.PartFromParetoFront, limit)
      pheromone.pheromoneUpdate(repository())
      assertDeposits(pheromone, deposits)
    }
  }

  test("reject single-front Pareto selection strategies for direct front mapping") {
    for (selection <- Vector(SolutionsSelectionStrategy.GlobalPareto, SolutionsSelectionStrategy.LastIterationPareto)) {
      intercept[IllegalArgumentException](table(UpdateType.PartFromParetoFront, selection = selection))
    }
  }

  test("handle empty archives and put equal objective values in evaluation bucket zero") {
    for (mode <- UpdateType.values) {
      val pheromone = table(mode)
      pheromone.pheromoneUpdate(new ParetoSolutionRepo(sameNodes, paretoFrontsCount = 1))
      assertDeposits(pheromone, Set.empty)
    }

    val repo = new ParetoSolutionRepo(sameNodes, paretoFrontsCount = 1)
    repo.addSolutions(0, Vector(solution(0, 2, 2), solution(1, 2, 2)))
    val pheromone = table(UpdateType.PartFromEvaluation, None)
    pheromone.pheromoneUpdate(repo)
    assertDeposits(pheromone, Set((0, 0, 0), (0, 1, 0), (1, 0, 0), (1, 1, 0)))
  }

  private def expectInvalid(action: => Any): Unit = {
    val rejected = try {
      action
      false
    } catch {
      case _: IllegalArgumentException => true
    }
    assert(rejected)
  }

  test("reject invalid bucket configurations before depositing pheromone") {
    expectInvalid(table(UpdateType.PartFromParetoFront, selection = SolutionsSelectionStrategy.LastIterationAll))
    expectInvalid(table(UpdateType.PartFromIndex, buckets = 0))
    val tooSmall = table(UpdateType.PartFromParetoFront, buckets = 2)
    expectInvalid(tooSmall.pheromoneUpdate(repository(fronts = 3)))
    assertDeposits(tooSmall, Set.empty, buckets = 2)
  }

  test("create the item table with Pareto front updates through the pheromone factory") {
    val config = PheromoneConfig(
      PheromoneType.ItemTwoDim.toString, 2, 0.25, 0.1, 0.0, 1.0, SolutionsSelectionStrategy.ParetoFronts.toString, 1,
      TwoDimPheromoneConfig(0, GetType.ExpectedCombination.toString, UpdateType.PartFromParetoFront.toString)
    )
    val pheromone = Pheromone.create(config, edges, 2, new Random(0), paretoFrontsCount = 4) match {
      case itemTable: ItemTwoDimPheromone => itemTable
      case _ => throw new AssertionError("Expected an item pheromone table")
    }
    assertEquals(pheromone.pheromone(0)(0).length, 4)
    pheromone.pheromoneUpdate(repository())
    assertDeposits(pheromone, Set((0, 0, 0), (0, 2, 1), (1, 1, 0), (1, 3, 1)))
  }
}
