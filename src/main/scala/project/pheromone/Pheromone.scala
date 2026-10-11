package project.pheromone

import project.config.PheromoneConfig
import project.config.PheromoneConfig.PheromoneType
import project.graph.Edge

import scala.util.Random

object Pheromone {
  /** For direct front mapping, pass the same front limit as the learning repository. */
  def create(
    config: PheromoneConfig,
    edges: Seq[Edge],
    optimizationTargetsCount: Int,
    rnd: Random,
    paretoFrontsCount: Int = 1
  ): BasePheromoneTable = {
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
