package project.pheromone

import project.config.PheromoneConfig
import project.graph.Edge

object Pheromone {
  def create(config: PheromoneConfig, edges: List[Edge], pheromoneDimension: Int): BasePheromoneTable = {
    config.tpe match {
      case "Basic" => BasicPheromoneTable(edges, config.increment, config.extinction, pheromoneDimension)
    }
  }
}
