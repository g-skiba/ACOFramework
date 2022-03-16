package project.pheromone

import project.graph.{Edge, Node}
import project.solution.BaseSolution

abstract class BasePheromone {

  /** Function to evaluate pheromone value per edge, based on solutions
   */
  def evaluate(solutions: List[BaseSolution]): Map[Edge, Double]

}
