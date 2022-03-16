package project.aggregator

import project.graph.{Edge, Node}
import project.pheromone.BasePheromone
import project.solution.BaseSolution

abstract class BasePheromoneAggregator(val pheromones: List[BasePheromone]) {

  /** Function to aggregate multiple pheromones in to single value per problem Edge
   */
  def evaluate(): Map[Edge, Double]

}
