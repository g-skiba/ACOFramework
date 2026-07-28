package project.pheromone

import project.config.SolutionsSelectionStrategy
import project.graph.Edge
import project.logging.DebugLogger.debug
import project.logging.WarnLogger.warn
import project.repo.BaseSolutionRepo

class ItemBasicPheromoneTable(
  edges: Seq[Edge],
  val increment: Double,
  val extinction: Double,
  val pheromoneDimension: Int,
  minValue: Double,
  maxValue: Double,
  solutionsSelectionStrategy: SolutionsSelectionStrategy,
  updateAnts: Option[Int]
) extends BasePheromoneTable {
  debug(s"Creating item basic pheromone table with $pheromoneDimension dimensions and $updateAnts update ants")

  private val maxItemNumber = edges.iterator.map(_.node2.number).max
  private val pheromone: Array[Array[Double]] =
    Array.fill(maxItemNumber + 1)(Array.fill(pheromoneDimension)(maxValue))

  override def getPheromone(edge: Edge): Array[Double] = pheromone(edge.node2.number)

  override def pheromoneUpdate(solutionsRepo: BaseSolutionRepo): Unit = {
    val solutions = solutionsSelectionStrategy(solutionsRepo)
    val takeSolutions = updateAnts.getOrElse(solutions.size)

    def updateDim(dim: Int): Unit = {
      val solutionsForDim = solutions.sortBy(_.evaluation(dim)).take(takeSolutions)
//      if (!updateAnts.forall(_ == solutionsForDim.size)) {
//        warn(s"Wanted: $updateAnts update ants, got ${solutionsForDim.size} solutions")
//      }
      debug(s"Updating item pheromone for dimension $dim using ${solutionsForDim.size} solutions: ${solutionsForDim.map(_.evaluation).sortBy(e => e.applyOrElse(dim, _ => e.head))}")
      solutionsForDim.foreach { solution =>
        solution.solution.iterator.drop(1).foreach { node =>
          val itemPheromones = pheromone(node.number)
          itemPheromones(dim) += increment
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
