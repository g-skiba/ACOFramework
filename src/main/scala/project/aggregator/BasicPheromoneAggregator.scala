package project.aggregator

import project.pheromone.BasePheromone
import project.solution.BaseSolution
import project.graph.Edge

class BasicPheromoneAggregator(pheromones: Seq[BasePheromone])
    extends BasePheromoneAggregator(pheromones) {

  override def evaluate(): Map[Edge, Double] = {
    for (pheromone <- pheromones) {
      ???
    }
    ???
  }
}
