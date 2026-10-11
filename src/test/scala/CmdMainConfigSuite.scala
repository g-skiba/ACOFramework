import project.config.{PheromoneConfig, SolutionsSelectionStrategy}
import project.config.PheromoneConfig.PheromoneType
import project.config.TwoDimPheromoneConfig.UpdateType
import project.graph.{Edge, Node}
import project.pheromone.{ItemTwoDimPheromone, Pheromone, TwoDimPheromone}

import scala.util.Random

class CmdMainConfigSuite extends munit.FunSuite {
  private val common = Vector("--pheromone-delta", "0.02", "--update-ants", "2")
  private val edges = Vector(Edge(Node(0), Node(1)))

  private def parse(tableType: PheromoneType, selection: SolutionsSelectionStrategy, extra: String*): PheromoneConfig =
    CmdMain.buildPheromoneConfig(new CmdMain.Config(
      common ++ Vector("--pheromone-type", tableType.toString, "--solutions-selection-strategy", selection.toString) ++ extra
    ))

  test("legacy CLI arguments retain defaults and pheromone-delta compatibility") {
    val basic = parse(PheromoneType.Basic, SolutionsSelectionStrategy.GlobalPareto)
    assertEquals(basic.paretoFrontsCount, 0)
    assertEquals(basic.paretoHistoryLength, 0)
    assertEquals(basic.increment, 0.02)
    assertEquals(basic.extinction, 0.02)
    val item = parse(PheromoneType.ItemTwoDim, SolutionsSelectionStrategy.GlobalPareto,
      "--two-dim-ph-size", "6", "--get-type", "ExpectedCombination", "--update-type", "PartFromIndex")
    assertEquals(item.twoDimConfig.twoDimSize, 6)
    assertEquals(item.resolveParetoFrontsCount, 0)
  }

  test("archive flags work for Basic tables and 2D Index/Evaluation with independent sizes") {
    val basic = parse(PheromoneType.ItemBasic, SolutionsSelectionStrategy.ParetoFronts,
      "--pareto-fronts-count", "3", "--pareto-history-length", "5")
    assertEquals(basic.resolveParetoFrontsCount, 3)
    assertEquals(basic.paretoHistoryLength, 5)
    Pheromone.create(basic, edges, 2, new Random(0))
    for (mode <- Vector(UpdateType.PartFromIndex, UpdateType.PartFromEvaluation)) {
      val twoDim = parse(PheromoneType.TwoDim, SolutionsSelectionStrategy.ParetoFronts,
        "--pareto-fronts-count", "3", "--pareto-history-length", "5",
        "--two-dim-ph-size", "6", "--get-type", "ExpectedCombination", "--update-type", mode.toString)
      assertEquals(twoDim.resolveParetoFrontsCount, 3)
      assertEquals(twoDim.twoDimConfig.twoDimSize, 6)
      val table = Pheromone.create(twoDim, edges, 2, new Random(0)).asInstanceOf[TwoDimPheromone]
      assertEquals(table.pheromone(0)(edges.head.cantorValue).length, 6)
    }
  }

  test("direct front mapping accepts omitted or explicit zero size and uses the configured front limit") {
    for {
      tableType <- Vector(PheromoneType.TwoDim, PheromoneType.ItemTwoDim)
      sizeArgs <- Vector(Vector.empty[String], Vector("--two-dim-ph-size", "0"))
    } {
      val args = Vector("--pareto-fronts-count", "4", "--get-type", "ExpectedCombination",
        "--update-type", "PartFromParetoFront") ++ sizeArgs
      val config = parse(tableType, SolutionsSelectionStrategy.ParetoFronts, args*)
      assertEquals(config.twoDimConfig.twoDimSize, 0)
      val table = Pheromone.create(config, edges, 2, new Random(0))
      val bucketCount = table match {
        case edgeTable: TwoDimPheromone => edgeTable.pheromone(0)(edges.head.cantorValue).length
        case itemTable: ItemTwoDimPheromone => itemTable.pheromone(0)(1).length
        case _ => throw new AssertionError("Expected a 2D pheromone table")
      }
      assertEquals(bucketCount, 4)
    }
  }

  test("reject missing archive limits, negative archive settings, and missing Index/Evaluation sizes") {
    intercept[IllegalArgumentException](parse(PheromoneType.Basic, SolutionsSelectionStrategy.ParetoFronts))
    intercept[IllegalArgumentException](parse(PheromoneType.Basic, SolutionsSelectionStrategy.GlobalPareto,
      "--pareto-fronts-count", "4"))
    for ((flag, value) <- Vector("--pareto-fronts-count" -> "-1", "--pareto-history-length" -> "-1")) {
      intercept[IllegalArgumentException](parse(PheromoneType.Basic, SolutionsSelectionStrategy.GlobalPareto, flag, value))
    }
    for (mode <- Vector(UpdateType.PartFromIndex, UpdateType.PartFromEvaluation)) {
      intercept[IllegalArgumentException](parse(PheromoneType.TwoDim, SolutionsSelectionStrategy.GlobalPareto,
        "--get-type", "ExpectedCombination", "--update-type", mode.toString))
    }
    val invalidMarker = parse(PheromoneType.TwoDim, SolutionsSelectionStrategy.ParetoFronts,
      "--pareto-fronts-count", "4", "--two-dim-ph-size", "2", "--get-type", "ExpectedCombination",
      "--update-type", "PartFromParetoFront")
    intercept[IllegalArgumentException](Pheromone.create(invalidMarker, edges, 2, new Random(0)))
  }
}
