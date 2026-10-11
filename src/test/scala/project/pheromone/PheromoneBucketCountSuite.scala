package project.pheromone

import project.config.SolutionsSelectionStrategy
import project.config.TwoDimPheromoneConfig.UpdateType

class PheromoneBucketCountSuite extends munit.FunSuite {
  test("direct front mapping derives a positive even bucket count from the archive limit") {
    for (fronts <- Vector(2, 4, 6)) {
      assertEquals(PheromoneUpdateSelection.resolveBucketCount(
        0, fronts, SolutionsSelectionStrategy.ParetoFronts, UpdateType.PartFromParetoFront
      ), fronts)
    }
  }

  test("Index and Evaluation retain explicit sizes independently of the archive limit and selection strategy") {
    for {
      mode <- Vector(UpdateType.PartFromIndex, UpdateType.PartFromEvaluation)
      selection <- SolutionsSelectionStrategy.values
      fronts <- Vector(1, 3, 7)
    } {
      assertEquals(PheromoneUpdateSelection.resolveBucketCount(4, fronts, selection, mode), 4)
    }
  }

  test("reject incompatible markers, resolved sizes, and selection strategies") {
    for (size <- Vector(-1, 2, 4)) {
      intercept[IllegalArgumentException](PheromoneUpdateSelection.resolveBucketCount(
        size, 4, SolutionsSelectionStrategy.ParetoFronts, UpdateType.PartFromParetoFront
      ))
    }
    for (fronts <- Vector(-2, 0, 1, 3)) {
      intercept[IllegalArgumentException](PheromoneUpdateSelection.resolveBucketCount(
        0, fronts, SolutionsSelectionStrategy.ParetoFronts, UpdateType.PartFromParetoFront
      ))
    }
    for (selection <- SolutionsSelectionStrategy.values.filter(_ != SolutionsSelectionStrategy.ParetoFronts)) {
      intercept[IllegalArgumentException](PheromoneUpdateSelection.resolveBucketCount(
        0, 4, selection, UpdateType.PartFromParetoFront
      ))
    }
    for {
      mode <- Vector(UpdateType.PartFromIndex, UpdateType.PartFromEvaluation)
      size <- Vector(-2, 0, 3)
    } {
      intercept[IllegalArgumentException](PheromoneUpdateSelection.resolveBucketCount(
        size, 4, SolutionsSelectionStrategy.ParetoFronts, mode
      ))
    }
  }
}
