package project.pheromone

import project.config.SolutionsSelectionStrategy
import project.graph.{Edge, Node}
import project.logging.DebugLogger.debug
import project.logging.WarnLogger.warn
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
    val solutions = solutionsSelectionStrategy(solutionsRepo)
    val takeSolutions = updateAnts.getOrElse(solutions.size)
    
    def updateDim(dim: Int): Unit = {
      val solutionsForDim = solutions.sortBy(_.evaluation(dim)).take(takeSolutions)
      if (!updateAnts.forall(_ == solutionsForDim.size)) {
        warn(s"Wanted: $updateAnts update ants, got ${solutionsForDim.size} solutions")
      }
      debug(s"Updating pheromone for dimension $dim using ${solutionsForDim.size} solutions: ${solutionsForDim.map(_.evaluation).sortBy(e => e.applyOrElse(dim, _ => e.head))}")
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

    for (dim <- 0 until pheromoneDimension) {
      updateDim(dim)
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
