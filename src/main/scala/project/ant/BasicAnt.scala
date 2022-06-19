package project.ant

import project.graph.{Edge, Node}
import project.decision.BaseDecisionAlgorithm
import project.problem.BaseProblem
import project.solution.BaseSolution

import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class BasicAnt(
    startingNode: Node,
    problem: BaseProblem,
    decision: BaseDecisionAlgorithm,
    val pheromoneWeights: Seq[Double],
    val heuristicWeights: Seq[Double]
) extends BaseAnt(
      startingNode,
      problem,
      decision
    ) {
  override def run(): BaseSolution = {
    val solution = ArrayBuffer.empty[Node]
    solution += problem.nodes.head

    @tailrec
    def iter(): BaseSolution = {
      val result = solution.toVector
      val selected = decision.decide(result, pheromoneWeights, heuristicWeights)
      selected match
        case Some(value) =>
          solution += value
          iter()
        case None =>
          BaseSolution(result, problem.evaluate(result))
    }

    iter()
  }

}
