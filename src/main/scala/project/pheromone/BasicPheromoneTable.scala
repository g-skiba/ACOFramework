package project.pheromone

import project.config.SolutionsSelectionStrategy
import project.graph.{Edge, Node}
import project.logging.DebugLogger.debug
import project.repo.BaseSolutionRepo
import project.solution.BaseSolution

import scala.collection.mutable.Map as MMap

class BasicPheromoneTable(
  edges: Seq[Edge],
  val increment: Double,
  val extinction: Double,
  val pheromoneDimension: Int,
  minValue: Double,
  maxValue: Double,
  solutionsSelectionStrategy: SolutionsSelectionStrategy,
  updateAnts: Option[Int]
) extends BasePheromoneTable {
  debug(s"Creating basic pheromone table with $pheromoneDimension dimensions and $updateAnts update ants")

  // indexed by edges' cantor value
  private val pheromone: Array[Array[Double]] = {
    val maxCantorValue = edges.iterator.map(e => e.cantorValue).max
    Array.fill(maxCantorValue + 1)(Array.fill(pheromoneDimension)(maxValue))
  }

  override def getPheromone(edge: Edge): Array[Double] = pheromone(edge.cantorValue)

  override def pheromoneUpdate(solutionsRepo: BaseSolutionRepo): Unit = {
    def updateDim(dim: Int, solutionsForDim: IndexedSeq[BaseSolution]): Unit = {
      require(
        updateAnts.forall(_ == solutionsForDim.size),
        s"Wanted: $updateAnts update ants, got ${solutionsForDim.size} solutions"
      )
      debug(s"Updating dimension $dim using ${solutionsForDim.size} solutions: ${solutionsForDim.map(_.evaluation).sortBy(e => e.applyOrElse(dim, _ => e.head))}")
      solutionsForDim.foreach { solution =>
          solution.solution
            .sliding(2)
            .map(x => Edge(x.head, x.last))
            .foreach { edge =>
              val edgePheromones = pheromone(edge.cantorValue)
              edgePheromones(dim) += increment
            }
        }
    }
    
    val solutions = solutionsSelectionStrategy(solutionsRepo)

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
    pheromone.foreach(_.mapInPlace(extinctAndEnsureMinMax))
  }

}
