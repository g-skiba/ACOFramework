package project.pheromone

import project.config.{PheromoneConfig, SolutionsSelectionStrategy}
import project.config.PheromoneConfig.PheromoneType
import project.graph.Edge

import scala.util.Random

object Pheromone {
  /** For ParetoFronts selection, pass the same positive front limit as the learning repository. */
  def create(
    config: PheromoneConfig,
    edges: Seq[Edge],
    optimizationTargetsCount: Int,
    rnd: Random,
    paretoFrontsCount: Int = 0
  ): BasePheromoneTable = {
    require(
      config.resolveSolutionsSelectionStrategy != SolutionsSelectionStrategy.ParetoFronts || paretoFrontsCount > 0,
      "ParetoFronts requires paretoFrontsCount > 0"
    )
    config.resolvePheromoneType match {
      case PheromoneType.Basic =>
        BasicPheromoneTable(
          edges,
          config.increment,
          config.extinction,
          config.resolvePheromoneDimension.getOrElse(optimizationTargetsCount),
          config.minValue,
          config.maxValue,
          config.resolveSolutionsSelectionStrategy,
          config.resolveTakenAntsToPheromoneUpdate
        )
      case PheromoneType.ItemBasic =>
        ItemBasicPheromoneTable(
          edges,
          config.increment,
          config.extinction,
          config.resolvePheromoneDimension.getOrElse(optimizationTargetsCount),
          config.minValue,
          config.maxValue,
          config.resolveSolutionsSelectionStrategy,
          config.resolveTakenAntsToPheromoneUpdate
        )
      case PheromoneType.TwoDim =>
        TwoDimPheromone(
          edges,
          config.increment,
          config.extinction,
          config.resolvePheromoneDimension.getOrElse(optimizationTargetsCount),
          config.minValue,
          config.maxValue,
          config.resolveSolutionsSelectionStrategy,
          config.resolveTakenAntsToPheromoneUpdate,
          PheromoneUpdateSelection.resolveBucketCount(
            config.twoDimConfig.twoDimSize, paretoFrontsCount,
            config.resolveSolutionsSelectionStrategy, config.twoDimConfig.resolveUpdateType
          ),
          config.twoDimConfig.resolveGetType,
          config.twoDimConfig.resolveUpdateType,
          rnd
        )
      case PheromoneType.ItemTwoDim =>
        ItemTwoDimPheromone(
          edges,
          config.increment,
          config.extinction,
          config.resolvePheromoneDimension.getOrElse(optimizationTargetsCount),
          config.minValue,
          config.maxValue,
          config.resolveSolutionsSelectionStrategy,
          config.resolveTakenAntsToPheromoneUpdate,
          PheromoneUpdateSelection.resolveBucketCount(
            config.twoDimConfig.twoDimSize, paretoFrontsCount,
            config.resolveSolutionsSelectionStrategy, config.twoDimConfig.resolveUpdateType
          ),
          config.twoDimConfig.resolveGetType,
          config.twoDimConfig.resolveUpdateType,
          rnd
        )
    }
  }
}
