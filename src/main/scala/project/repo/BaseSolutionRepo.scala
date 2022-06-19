package project.repo

import project.solution.BaseSolution
import scala.collection.mutable.{Map => MMap}

abstract class BaseSolutionRepo(val solutions: MMap[Int, Seq[BaseSolution]]) {

  def addSolutions(iteration: Int, newSolutions: Seq[BaseSolution]): Unit
}
