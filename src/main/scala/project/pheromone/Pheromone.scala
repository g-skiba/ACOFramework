package project.pheromone

import project.config.PheromoneConfig
import project.config.PheromoneConfig.PheromoneType
import project.graph.Edge

object Pheromone {
  def create(
      config: PheromoneConfig,
      edges: Seq[Edge],
      optimizationTargetsCount: Int
  ): BasePheromoneTable = {
    config.resolvePheromoneType match {
      case PheromoneType.Basic =>
        project.pheromone.BasicPheromoneTable(
          edges,
          config.increment,
          config.extinction,
          config.resolvePheromoneDimension.getOrElse(optimizationTargetsCount),
          config.minValue,
          config.maxValue
        )
      case PheromoneType.TwoDim =>
        project.pheromone.TwoDimPheromone(
          edges,
          config.increment,
          config.extinction,
          config.resolvePheromoneDimension.getOrElse(optimizationTargetsCount),
          config.minValue,
          config.maxValue,
          config.twoDimConfig.twoDimSize,
          config.twoDimConfig.resolveGetType,
          config.twoDimConfig.resolveUpdateType
        )
    }
  }
}
