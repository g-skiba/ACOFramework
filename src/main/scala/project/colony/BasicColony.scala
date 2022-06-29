package project.colony

import project.ant.{BaseAnt, BasicAnt}
import project.decision.BasicDecisionAlgorithm
import project.graph.{Edge, Node}
import project.pheromone.BasePheromoneTable
import project.problem.BaseProblem
import project.solution.BaseSolution

import scala.collection.mutable.ListBuffer
import scala.util.Random

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
    val startingNode = problem.nodes.head

    for (_ <- 0 until antNumb) {
      ants.append(
        new BasicAnt(
          startingNode = startingNode,
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

  override def run(): IndexedSeq[BaseSolution] = {
    val solutions = Vector.newBuilder[BaseSolution]
    for (ant <- ants) {
      val solution: BaseSolution = ant.run()
      solutions += solution
    }
    solutions.result()
  }

  def pheromoneUpdate(solutions: IndexedSeq[BaseSolution]): Unit = {
    pheromoneTable.pheromoneUpdate(solutions)
    pheromoneTable.afterUpdatesAction()
  }
}
