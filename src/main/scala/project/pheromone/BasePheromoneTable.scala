package project.pheromone

import project.graph.{Edge, Node}
import project.repo.BaseSolutionRepo
import project.solution.BaseSolution

abstract class BasePheromoneTable {

  def getPheromone(edge: Edge): Array[Double]

  def pheromoneUpdate(solutionsRepo: BaseSolutionRepo): Unit

  def afterUpdatesAction(): Unit

}
