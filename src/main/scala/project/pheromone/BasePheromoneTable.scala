package project.pheromone

import project.graph.{Edge, Node}
import project.solution.BaseSolution

abstract class BasePheromoneTable {

  def getPheromone(edge: Edge): Seq[Double]

  def pheromoneUpdate(solutions: Seq[BaseSolution]): Unit

  def afterUpdatesAction(): Unit

}
