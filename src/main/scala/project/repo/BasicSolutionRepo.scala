package project.repo

import project.solution.BaseSolution
import scala.collection.mutable.{Map => MMap}
class BasicSolutionRepo extends BaseSolutionRepo(MMap[Int, List[BaseSolution]]()) {

  override def addSolutions(iteration: Int, newSolutions: List[BaseSolution]): Unit = {
    solutions.update(iteration, newSolutions)
  }

  override def toString: String = {
    var string = ""
    for (key <- solutions.keysIterator) {
      string = string.concat("Iteration: \n" + key.toString + "\n")
      val bestSolution = solutions(key).minBy(_.evaluation.sum)
      string = string.concat(
        "Best Solution: \n" + bestSolution.solution.toString + "\n"
      )
      string = string.concat(
        "Evaluation: \n" + bestSolution.evaluation.toString + "\n"
      )
    }
    string
  }
}
