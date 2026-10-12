package project.config

import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

class PheromoneArchiveConfigSuite extends munit.FunSuite {
  private def load(input: String): PheromoneConfig =
    new Yaml(new Constructor(classOf[ProblemConfig])).load[ProblemConfig](input).algorithmConfig.pheromoneConfig

  test("YAML and constructor calls without archive settings default to a disabled learning archive") {
    val fromYaml = load("""algorithmConfig:
      |  pheromoneConfig:
      |    solutionsSelectionStrategy: GlobalPareto
      |""".stripMargin)
    val constructor = PheromoneConfig("Basic", 2, 0.1, 0.1, 0.0, 1.0, "GlobalPareto", 1, new TwoDimPheromoneConfig())
    for (config <- Vector(fromYaml, constructor, new PheromoneConfig())) {
      assertEquals(config.paretoFrontsCount, 0)
      assertEquals(config.paretoHistoryLength, 0)
      assertEquals(config.resolveParetoFrontsCount, 0)
    }
  }

  test("load archive settings under pheromoneConfig and record them in configuration metadata") {
    val config = load("""algorithmConfig:
      |  pheromoneConfig:
      |    pheromoneTpe: ItemTwoDim
      |    solutionsSelectionStrategy: ParetoFronts
      |    paretoFrontsCount: 4
      |    paretoHistoryLength: 20
      |    twoDimConfig:
      |      twoDimSize: 0
      |      twoDimGetType: ExpectedCombination
      |      twoDimUpdateType: PartFromParetoFront
      |""".stripMargin)
    assertEquals(config.resolveSolutionsSelectionStrategy, SolutionsSelectionStrategy.ParetoFronts)
    assertEquals(config.resolveParetoFrontsCount, 4)
    assertEquals(config.paretoHistoryLength, 20)
    assertEquals(config.twoDimConfig.twoDimSize, 0)
    assertEquals(config.toMap("paretoFrontsCount"), "4")
    assertEquals(config.toMap("paretoHistoryLength"), "20")
    assertEquals(config.toCsv.split(";").takeRight(2).toVector, Vector("4", "20"))
    assert(config.toString.contains("paretoFrontsCount: 4"))
    assert(config.toString.contains("paretoHistoryLength: 20"))
  }

  test("validate archive settings and require zero fronts for other selectors") {
    val config = new PheromoneConfig()
    intercept[IllegalArgumentException](config.copy(paretoFrontsCount = -1).validateLearningArchive())
    intercept[IllegalArgumentException](config.copy(paretoHistoryLength = -1).validateLearningArchive())
    intercept[IllegalArgumentException](config.copy(solutionsSelectionStrategy = "ParetoFronts").validateLearningArchive())
    for (selection <- SolutionsSelectionStrategy.values.filter(_ != SolutionsSelectionStrategy.ParetoFronts)) {
      assertEquals(config.copy(solutionsSelectionStrategy = selection.toString).resolveParetoFrontsCount, 0)
      val error = intercept[IllegalArgumentException] {
        config.copy(solutionsSelectionStrategy = selection.toString, paretoFrontsCount = 3).validateLearningArchive()
      }
      assert(error.getMessage.contains("must be 0"))
    }
    assertEquals(config.copy(solutionsSelectionStrategy = "ParetoFronts", paretoFrontsCount = 3).resolveParetoFrontsCount, 3)
  }
}
