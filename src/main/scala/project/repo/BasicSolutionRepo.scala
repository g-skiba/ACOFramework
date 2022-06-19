package project.repo

import project.solution.BaseSolution
import scala.collection.mutable.{Map => MMap}
class BasicSolutionRepo extends BaseSolutionRepo(MMap[Int, Seq[BaseSolution]]()) {

  override def addSolutions(iteration: Int, newSolutions: Seq[BaseSolution]): Unit = {
    solutions.update(iteration, newSolutions)
  }

  override def toString: String = {
    var string = new StringBuilder
    for (key <- solutions.keysIterator) {
      string = string.append(s"Iteration: $key\n")
      val bestSolution = solutions(key).minBy(_.evaluation.sum)
      string = string.append(s"Best Solution: ${bestSolution.solution}\n")
      string = string.append(s"Evaluation: ${bestSolution.evaluation}\n")
    }
    string.append(s"Best ever cost: ${solutions.values.flatten.map(_.evaluation.sum).min}")
    string.result()
  }
}
