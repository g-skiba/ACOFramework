package project.pheromone

import project.config.{PheromoneConfig, SolutionsSelectionStrategy, TwoDimPheromoneConfig}
import project.config.PheromoneConfig.PheromoneType
import project.config.TwoDimPheromoneConfig.{GetType, UpdateType}
import project.graph.{Edge, Node}

import scala.util.Random

class PheromoneArchiveValidationSuite extends munit.FunSuite {
  private val edges = Vector(Edge(Node(0), Node(1)))

  private def config(tableType: PheromoneType, selection: SolutionsSelectionStrategy, mode: UpdateType): PheromoneConfig =
    PheromoneConfig(
      tableType.toString, 2, 0.25, 0.1, 0.0, 1.0, selection.toString, 1,
      TwoDimPheromoneConfig(
        if (mode == UpdateType.PartFromParetoFront) 0 else 2,
        GetType.ExpectedCombination.toString, mode.toString
      )
    )

  test("existing selections create every table without a learning archive") {
    for {
      tableType <- PheromoneType.values
      selection <- SolutionsSelectionStrategy.values.filter(_ != SolutionsSelectionStrategy.ParetoFronts)
      mode <- Vector(UpdateType.PartFromIndex, UpdateType.PartFromEvaluation)
    } {
      val table = Pheromone.create(config(tableType, selection, mode), edges, 2, new Random(0))
      assertEquals(table.getPheromone(edges.head).toVector, Vector(1.0, 1.0))
    }
  }

  test("ParetoFronts requires an explicit positive archive limit for every table and update mode") {
    for {
      tableType <- PheromoneType.values
      mode <- UpdateType.values
    } {
      val cfg = config(tableType, SolutionsSelectionStrategy.ParetoFronts, mode)
      val omitted = intercept[IllegalArgumentException](Pheromone.create(cfg, edges, 2, new Random(0)))
      assert(omitted.getMessage.contains("paretoFrontsCount > 0"))
      for (fronts <- Vector(0, -1)) {
        intercept[IllegalArgumentException](Pheromone.create(cfg.copy(paretoFrontsCount = fronts), edges, 2, new Random(0)))
      }
      Pheromone.create(cfg.copy(paretoFrontsCount = 2), edges, 2, new Random(0))
    }
  }
}
