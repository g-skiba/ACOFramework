package project.pheromone

import project.graph.{Edge, Node}
import project.solution.BaseSolution

abstract class BasePheromoneTable {

  def getPheromone(edge: Edge): List[Double]

  def pheromoneUpdate(solution: BaseSolution): Unit

  def afterUpdatesAction(): Unit

}
