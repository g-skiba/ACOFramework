package project.pheromone

import project.graph.{Edge, Node}
import project.repo.BaseSolutionRepo
import project.solution.BaseSolution

import scala.collection.mutable.Map as MMap

//wiele macierzy feromonów nie ma sensu przy takiej implementacji updatu feromonów
class BasicPheromoneTable(
    edges: Seq[Edge],
    val increment: Double,
    val extinction: Double,
    val pheromoneDimension: Int,
    minValue: Double,
    maxValue: Double,
    updateAnts: Option[Int]
) extends BasePheromoneTable {
  private val pheromone: MMap[Edge, Array[Double]] =
    edges.map((_, Array.fill(pheromoneDimension)(maxValue))).to(MMap)

  override def getPheromone(edge: Edge): Array[Double] = pheromone(edge)

  override def pheromoneUpdate(solutionsRepo: BaseSolutionRepo): Unit = {
    val solutions = solutionsRepo.solutionsForLastIteration

    def updateDim(dim: Int, solutionsForDim: Seq[BaseSolution]): Unit = {
      solutionsForDim.foreach { solution =>
          solution.solution
            .sliding(2)
            .map(x => Edge(x.head, x.last))
            .foreach { edge =>
              val edgePheromones = pheromone(edge)
              edgePheromones(dim) += increment
            }
        }
    }

    def optionallySortedSolutions(dim: Int): IndexedSeq[BaseSolution] = {
      updateAnts.map(_ => solutions.sortBy(_.evaluation(dim))).getOrElse(solutions)
    }

    val takeSolutions = updateAnts.getOrElse(solutions.size)
    pheromoneDimension match {
      case 1 =>
        updateDim(0, optionallySortedSolutions(0).take(takeSolutions))
      case 2 =>
        val optionallySorted = optionallySortedSolutions(0)
        updateDim(0, optionallySorted.take(takeSolutions))
        updateDim(1, optionallySorted.takeRight(takeSolutions))
      case _ =>
        for (dim <- 0 until pheromoneDimension) {
          val optionallySorted = optionallySortedSolutions(dim)
          updateDim(dim, optionallySorted.take(takeSolutions))
        }
    }
  }

  override def afterUpdatesAction(): Unit = {
    def extinctAndEnsureMinMax(double: Double): Double = {
      val e = double * (1 - extinction)
      e.min(maxValue).max(minValue)
    }
    pheromone.mapValuesInPlace((_, values) =>
      values.map(extinctAndEnsureMinMax)
    )
  }

}
