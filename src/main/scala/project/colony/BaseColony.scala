package project.colony

import project.aggregator.BasicPheromoneAggregator
import project.ant.BaseAnt
import project.pheromone.BasePheromoneTable
import project.problem.BaseProblem
import project.solution.BaseSolution

abstract class BaseColony(
  antNumb: Int,
  problem: BaseProblem,
  val pheromoneTable: BasePheromoneTable
) {
  val ants: List[BaseAnt] = createAnts()
  // val pheromoneAggregator = new BasicPheromoneAggregator()
  /** Function that runs all ants to create solutions
    */
  def run(): IndexedSeq[BaseSolution]

  /** Function to place ants
    */
  def createAnts(): List[BaseAnt]
}
