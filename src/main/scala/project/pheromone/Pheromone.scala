package project.pheromone

import project.config.PheromoneConfig
import project.config.PheromoneConfig.PheromoneType
import project.graph.Edge

import scala.util.Random

object Pheromone {
  def create(
    config: PheromoneConfig,
    edges: Seq[Edge],
    optimizationTargetsCount: Int,
    rnd: Random  
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
          config.twoDimConfig.twoDimSize,
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
          config.twoDimConfig.twoDimSize,
          config.twoDimConfig.resolveGetType,
          config.twoDimConfig.resolveUpdateType,
          rnd
        )
    }
  }
}
