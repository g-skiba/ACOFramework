package project.colony

import project.graph.{Edge, Node}
import project.ant.{BaseAnt, BasicAnt}
import project.decision.BasicDecisionAlgorithm
import project.problem.BaseProblem
import project.solution.BaseSolution

import scala.collection.mutable.ListBuffer
import scala.util.Random
import project.pheromone.BasePheromoneTable

class BasicColony(
  alpha: Double,
  beta: Double,
  random: Random,
  antNumb: Int,
  problem: BaseProblem,
  pheromoneTable: BasePheromoneTable,
  heuristicWeights: List[Double],
  pheromoneWeights: List[Double]
) extends BaseColony(antNumb, problem, pheromoneTable) {

  override def createAnts(): List[BaseAnt] = {
    val ants = ListBuffer[BaseAnt]()

    for (_ <- 0 until antNumb) {
      ants.append(
        new BasicAnt(
          startingNode = problem.startingNode,
          problem = problem,
          decision = new BasicDecisionAlgorithm(
            alpha,
            beta,
            problem,
            pheromoneTable,
            random
          ),
          heuristicWeights = heuristicWeights,
          pheromoneWeights = pheromoneWeights
        )
      )
    }
    ants.toList
  }

  override def run(): List[BaseSolution] = {
    val solutions = ListBuffer[BaseSolution]()
    for (ant <- ants) {
      val solution: BaseSolution = ant.run()
      solutions.append(solution)
    }
    solutions.toList
  }

  def pheromoneUpdate(solutions: List[BaseSolution]): Unit = {
    pheromoneTable.pheromoneUpdate(solutions)
    pheromoneTable.afterUpdatesAction()
  }
}
