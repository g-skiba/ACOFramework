package project.pheromone

import project.config.{PheromoneConfig, SolutionsSelectionStrategy, TwoDimPheromoneConfig}
import project.config.PheromoneConfig.PheromoneType
import project.config.TwoDimPheromoneConfig.{GetType, UpdateType}
import project.graph.{Edge, Node}
import project.repo.ParetoSolutionRepo
import project.solution.BaseSolution

import scala.util.Random

class PheromoneBucketParitySuite extends munit.FunSuite {
  private val edges = (1 to 3).map(item => Edge(Node(0), Node(item))).toVector
  private val tableTypes = Vector(PheromoneType.TwoDim, PheromoneType.ItemTwoDim)

  private def config(tableType: PheromoneType, mode: UpdateType, getType: GetType, buckets: Int): PheromoneConfig =
    PheromoneConfig(
      tableType.toString, 2, 0.1, 0.2, 0.01, 1.0, SolutionsSelectionStrategy.ParetoFronts.toString, 1,
      TwoDimPheromoneConfig(if (mode == UpdateType.PartFromParetoFront) 0 else buckets, getType.toString, mode.toString),
      paretoFrontsCount = if (mode == UpdateType.PartFromParetoFront) buckets else 3
    )

  private def firstEdgeBuckets(table: BasePheromoneTable): Array[Double] = table match {
    case edgeTable: TwoDimPheromone => edgeTable.pheromone(0)(edges.head.cantorValue)
    case itemTable: ItemTwoDimPheromone => itemTable.pheromone(0)(edges.head.node2.number)
    case _ => throw new AssertionError("Expected a 2D pheromone table")
  }

  test("odd bucket counts support deposits, evaporation, and reads in both table implementations") {
    for {
      tableType <- tableTypes
      mode <- UpdateType.values
      getType <- GetType.values.filter(_ != GetType.PairingCombination)
      buckets <- Vector(1, 3)
    } {
      val cfg = config(tableType, mode, getType, buckets)
      val table = Pheromone.create(cfg, edges, 2, new Random(0))
      val repo = new ParetoSolutionRepo((first, second) => first == second, paretoFrontsCount = cfg.paretoFrontsCount)
      repo.addSolutions(0, edges.zipWithIndex.map { case (edge, index) =>
        BaseSolution(Vector(edge.node1, edge.node2), Vector(index + 1.0, index + 1.0))
      })

      assertEquals(firstEdgeBuckets(table).length, buckets)
      table.pheromoneUpdate(repo)
      assertEquals(firstEdgeBuckets(table).head, 1.1)
      if (mode == UpdateType.PartFromParetoFront && buckets == 3) {
        val lastFrontDeposit = table match {
          case edgeTable: TwoDimPheromone => edgeTable.pheromone(0)(edges.last.cantorValue)(2)
          case itemTable: ItemTwoDimPheromone => itemTable.pheromone(0)(edges.last.node2.number)(2)
          case _ => throw new AssertionError("Expected a 2D pheromone table")
        }
        assertEquals(lastFrontDeposit, 1.1)
      }
      table.afterUpdatesAction()
      for (edge <- edges; value <- table.getPheromone(edge)) {
        assert(!value.isNaN && !value.isInfinity && value > 0.0 && value <= 1.0,
          s"Invalid read for $tableType, $mode, $getType, $buckets buckets: $value")
      }
    }
  }

  test("the factory applies PairingCombination parity to both explicit and derived sizes") {
    for (tableType <- tableTypes; mode <- UpdateType.values) {
      val error = intercept[IllegalArgumentException] {
        Pheromone.create(config(tableType, mode, GetType.PairingCombination, 3), edges, 2, new Random(0))
      }
      assert(error.getMessage.contains("PairingCombination"))
      val evenTable = Pheromone.create(config(tableType, mode, GetType.PairingCombination, 2), edges, 2, new Random(0))
      assertEquals(evenTable.getPheromone(edges.head).toVector, Vector(1.0, 1.0))
    }
  }

  test("direct constructors also reject odd PairingCombination sizes") {
    for (mode <- UpdateType.values) {
      intercept[IllegalArgumentException] {
        new TwoDimPheromone(edges, 0.1, 0.2, 2, 0.01, 1.0, SolutionsSelectionStrategy.ParetoFronts,
          Some(1), 3, GetType.PairingCombination, mode, new Random(0))
      }
      intercept[IllegalArgumentException] {
        new ItemTwoDimPheromone(edges, 0.1, 0.2, 2, 0.01, 1.0, SolutionsSelectionStrategy.ParetoFronts,
          Some(1), 3, GetType.PairingCombination, mode, new Random(0))
      }
    }
  }
}
