package project.pheromone

import project.config.{PheromoneConfig, SolutionsSelectionStrategy, TwoDimPheromoneConfig}
import project.config.PheromoneConfig.PheromoneType
import project.config.TwoDimPheromoneConfig.{GetType, UpdateType}
import project.graph.{Edge, Node}
import project.repo.ParetoSolutionRepo
import project.solution.BaseSolution

import scala.util.Random

class TwoDimParetoFrontUpdateSuite extends munit.FunSuite {
  private val sameNodes: (Seq[Node], Seq[Node]) => Boolean = (first, second) => first == second
  private val edges = (1 to 6).map(id => Edge(Node(0), Node(id))).toVector
  private val solutions = Vector(
    solution(1, 1, 5), solution(2, 5, 1),
    solution(3, 2, 6), solution(4, 6, 2),
    solution(5, 3, 7), solution(6, 7, 3)
  )

  private def solution(id: Int, first: Double, second: Double): BaseSolution =
    BaseSolution(Vector(Node(0), Node(id)), Vector(first, second))

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
  ): TwoDimPheromone = new TwoDimPheromone(
    edges, 0.25, 0.1, 2, 0.0, 1.0, selection, limit, buckets, GetType.ExpectedCombination, mode, new Random(0)
  )

  private def assertDeposits(pheromone: TwoDimPheromone, deposits: Set[(Int, Int, Int)], buckets: Int = 4): Unit = {
    for {
      dim <- 0 until 2
      edgeIndex <- edges.indices
      bucket <- 0 until buckets
    } {
      val expected = if (deposits.contains((dim, edgeIndex, bucket))) 1.25 else 1.0
      assert(pheromone.pheromone(dim)(edges(edgeIndex).cantorValue)(bucket) == expected,
        s"Unexpected deposit for objective $dim, edge $edgeIndex, bucket $bucket")
    }
  }

  test("select per objective within every front and use the same contributors for all bucket modes") {
    val repo = repository()
    assert(repo.learningParetoFronts == Vector(solutions.take(2), solutions.slice(2, 4)))
    val frontDeposits = Set((0, 0, 0), (0, 2, 1), (1, 1, 0), (1, 3, 1))
    val frontTable = table(UpdateType.PartFromParetoFront)
    frontTable.pheromoneUpdate(repo)
    assertDeposits(frontTable, frontDeposits)

    val indexTable = table(UpdateType.PartFromIndex)
    indexTable.pheromoneUpdate(repo)
    assertDeposits(indexTable, Set((0, 0, 0), (0, 2, 2), (1, 1, 0), (1, 3, 2)))

    val evaluationTable = table(UpdateType.PartFromEvaluation)
    evaluationTable.pheromoneUpdate(repo)
    assertDeposits(evaluationTable, Set((0, 0, 0), (0, 2, 3), (1, 1, 0), (1, 3, 3)))

    // Missing fronts leave their buckets without deposits, but evaporation still applies.
    frontTable.afterUpdatesAction()
    for {
      dim <- 0 until 2
      edge <- edges
      bucket <- 2 until 4
    } assert(math.abs(frontTable.pheromone(dim)(edge.cantorValue)(bucket) - 0.9) < 1e-12)
  }

  test("use all retained solutions when the limit is absent or nonpositive") {
    val deposits = (for {
      dim <- 0 until 2
      edgeIndex <- 0 until 4
    } yield (dim, edgeIndex, edgeIndex / 2)).toSet
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

    val equalRepo = new ParetoSolutionRepo(sameNodes, paretoFrontsCount = 1)
    equalRepo.addSolutions(0, Vector(solution(1, 2, 2), solution(2, 2, 2)))
    val pheromone = table(UpdateType.PartFromEvaluation, None)
    pheromone.pheromoneUpdate(equalRepo)
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

  test("preserve front indices across empty groups") {
    val groups = Vector(Vector.empty[BaseSolution], Vector(solutions(1), solutions(0)), Vector.empty[BaseSolution], Vector(solutions(2)))
    val selected = PheromoneUpdateSelection.selectForObjective(groups, 0, Some(1))
    assert(selected == Vector(Vector.empty, Vector(solutions(0)), Vector.empty, Vector(solutions(2))))
    assert(PheromoneUpdateSelection.assignBuckets(selected, 0, UpdateType.PartFromParetoFront, 4) ==
      Vector((solutions(0), 1), (solutions(2), 3)))
  }

  test("reject invalid bucket configurations before depositing pheromone") {
    expectInvalid(table(UpdateType.PartFromParetoFront, selection = SolutionsSelectionStrategy.LastIterationAll))
    expectInvalid(table(UpdateType.PartFromIndex, buckets = 0))
    val tooSmall = table(UpdateType.PartFromParetoFront, buckets = 2)
    expectInvalid(tooSmall.pheromoneUpdate(repository(fronts = 3)))
    assertDeposits(tooSmall, Set.empty, buckets = 2)
  }

  test("resolve the front limit to an actual edge table size through the pheromone factory") {
    val config = PheromoneConfig(
      PheromoneType.TwoDim.toString, 2, 0.25, 0.1, 0.0, 1.0, SolutionsSelectionStrategy.ParetoFronts.toString, 1,
      TwoDimPheromoneConfig(0, GetType.ExpectedCombination.toString, UpdateType.PartFromParetoFront.toString)
    )
    val pheromone = Pheromone.create(config, edges, 2, new Random(0), paretoFrontsCount = 4) match {
      case edgeTable: TwoDimPheromone => edgeTable
      case _ => throw new AssertionError("Expected an edge pheromone table")
    }
    assertEquals(pheromone.pheromone(0)(edges.head.cantorValue).length, 4)
    pheromone.pheromoneUpdate(repository(fronts = 3))
    assertDeposits(pheromone, Set((0, 0, 0), (0, 2, 1), (0, 4, 2), (1, 1, 0), (1, 3, 1), (1, 5, 2)))
  }
}
