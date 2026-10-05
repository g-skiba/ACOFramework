package project.pheromone

import project.config.SolutionsSelectionStrategy
import project.config.TwoDimPheromoneConfig.{GetType, UpdateType}
import project.graph.{Edge, Node}
import project.repo.ParetoSolutionRepo
import project.solution.BaseSolution

import scala.util.Random

/** Run with `sbt "Test/runMain project.pheromone.TwoDimParetoFrontUpdateCheck"`. */
object TwoDimParetoFrontUpdateCheck {
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

  def main(args: Array[String]): Unit = {
    selectsPerObjectiveWithinEveryFront()
    selectsAllWithoutAPositiveLimit()
    acceptsExistingParetoStrategies()
    handlesEmptyArchivesAndEqualValues()
    validatesFrontMapping()
    println("TwoDim Pareto front update checks passed")
  }

  private def selectsPerObjectiveWithinEveryFront(): Unit = {
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

  private def selectsAllWithoutAPositiveLimit(): Unit = {
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

  private def acceptsExistingParetoStrategies(): Unit = {
    for (selection <- Vector(SolutionsSelectionStrategy.GlobalPareto, SolutionsSelectionStrategy.LastIterationPareto)) {
      val pheromone = table(UpdateType.PartFromParetoFront, selection = selection)
      pheromone.pheromoneUpdate(repository())
      assertDeposits(pheromone, Set((0, 0, 0), (1, 1, 0)))
    }
  }

  private def handlesEmptyArchivesAndEqualValues(): Unit = {
    for (mode <- UpdateType.values) {
      val pheromone = table(mode)
      pheromone.pheromoneUpdate(new ParetoSolutionRepo(sameNodes))
      assertDeposits(pheromone, Set.empty)
    }

    val equalRepo = new ParetoSolutionRepo(sameNodes)
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

  private def validatesFrontMapping(): Unit = {
    expectInvalid(table(UpdateType.PartFromParetoFront, selection = SolutionsSelectionStrategy.LastIterationAll))
    expectInvalid(table(UpdateType.PartFromIndex, buckets = 0))
    val tooSmall = table(UpdateType.PartFromParetoFront, buckets = 2)
    expectInvalid(tooSmall.pheromoneUpdate(repository(fronts = 3)))
    assertDeposits(tooSmall, Set.empty, buckets = 2)
  }

}
