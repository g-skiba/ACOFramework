package project.pheromone

import project.config.SolutionsSelectionStrategy
import project.config.TwoDimPheromoneConfig.{GetType, UpdateType}

class PheromoneBucketCountSuite extends munit.FunSuite {
  test("direct front mapping derives the bucket count from the archive limit for each read method") {
    for {
      getType <- GetType.values
      fronts <- Vector(1, 2, 3, 4, 6)
      if getType != GetType.PairingCombination || fronts % 2 == 0
    } {
      assertEquals(PheromoneUpdateSelection.resolveBucketCount(
        0, fronts, SolutionsSelectionStrategy.ParetoFronts, UpdateType.PartFromParetoFront, getType
      ), fronts)
    }
  }

  test("Index and Evaluation retain explicit sizes independently of the archive limit and selection strategy") {
    for {
      mode <- Vector(UpdateType.PartFromIndex, UpdateType.PartFromEvaluation)
      selection <- SolutionsSelectionStrategy.values
      fronts <- Vector(1, 3, 7)
      getType <- GetType.values
      size <- Vector(3, 4)
      if getType != GetType.PairingCombination || size % 2 == 0
    } {
      assertEquals(PheromoneUpdateSelection.resolveBucketCount(size, fronts, selection, mode, getType), size)
    }
  }

  test("reject incompatible markers, resolved sizes, and selection strategies") {
    for (size <- Vector(-1, 2, 4)) {
      intercept[IllegalArgumentException](PheromoneUpdateSelection.resolveBucketCount(
        size, 4, SolutionsSelectionStrategy.ParetoFronts, UpdateType.PartFromParetoFront, GetType.ExpectedCombination
      ))
    }
    for (fronts <- Vector(-2, 0)) {
      intercept[IllegalArgumentException](PheromoneUpdateSelection.resolveBucketCount(
        0, fronts, SolutionsSelectionStrategy.ParetoFronts, UpdateType.PartFromParetoFront, GetType.ExpectedCombination
      ))
    }
    for (selection <- SolutionsSelectionStrategy.values.filter(_ != SolutionsSelectionStrategy.ParetoFronts)) {
      intercept[IllegalArgumentException](PheromoneUpdateSelection.resolveBucketCount(
        0, 4, selection, UpdateType.PartFromParetoFront, GetType.ExpectedCombination
      ))
    }
    for {
      mode <- Vector(UpdateType.PartFromIndex, UpdateType.PartFromEvaluation)
      size <- Vector(-2, 0)
    } {
      intercept[IllegalArgumentException](PheromoneUpdateSelection.resolveBucketCount(
        size, 4, SolutionsSelectionStrategy.ParetoFronts, mode, GetType.ExpectedCombination
      ))
    }
  }

  test("PairingCombination rejects odd actual bucket counts for every update mode") {
    for {
      mode <- UpdateType.values
      size <- Vector(1, 3, 5)
    } {
      val configuredSize = if (mode == UpdateType.PartFromParetoFront) 0 else size
      val error = intercept[IllegalArgumentException](PheromoneUpdateSelection.resolveBucketCount(
        configuredSize, size, SolutionsSelectionStrategy.ParetoFronts, mode, GetType.PairingCombination
      ))
      assert(error.getMessage.contains("PairingCombination"))
    }
  }
}
