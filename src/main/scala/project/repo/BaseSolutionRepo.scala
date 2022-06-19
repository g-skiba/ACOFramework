package project.repo

import project.solution.BaseSolution
import scala.collection.mutable.{Map => MMap}

abstract class BaseSolutionRepo {
  protected val solutions: MMap[Int, Seq[BaseSolution]] = MMap.empty

  def addSolutions(iteration: Int, newSolutions: IndexedSeq[BaseSolution]): IndexedSeq[BaseSolution]
  def globalBest: Seq[BaseSolution]

  def solutionsForIteration(it: Int): Seq[BaseSolution] = {
    solutions.getOrElse(it, List.empty)
  }

  def solutionsIterator: Iterator[Seq[BaseSolution]] = {
    solutions.iterator.map(_._2)
  }

  override def toString: String = {
    var string = new StringBuilder
    for (key <- solutions.keysIterator) {
      string = string.append(s"Iteration: $key; ")
      val itSolutions = solutions(key)
      val bestSolutions = itSolutions.drop(1).foldLeft(itSolutions.head.evaluation) { case (best, solution) =>
        best.zip(solution.evaluation).map { case (v1, v2) => math.min(v1, v2) }
      }
      string = string.append(s"Best by target: ${bestSolutions.mkString(", ")}\n")
    }
    string.append(s"Global best solutions: ${globalBest.map(_.evaluation).sortBy(_.head).map(_.mkString("[", ",", "]")).mkString("; ")}")
    string.result()
  }
}
