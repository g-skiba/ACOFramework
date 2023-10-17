package project.pheromone

import project.graph.{Edge, Node}
import project.solution.BaseSolution

import scala.collection.mutable.Map as MMap

//wiele macierzy feromonów nie ma sensu przy takiej implementacji updatu feromonów
class BasicPheromoneTable(
    edges: Seq[Edge],
    val increment: Double,
    val extinction: Double,
    val pheromoneDimension: Int,
    minValue: Double,
    maxValue: Double
) extends BasePheromoneTable {
  private val pheromone: MMap[Edge, Array[Double]] =
    edges.map((_, Array.fill(pheromoneDimension)(maxValue))).to(MMap)

  override def getPheromone(edge: Edge): Array[Double] = pheromone(edge)

  override def pheromoneUpdate(solutions: Seq[BaseSolution]): Unit = {
    solutions.foreach { solution =>
      solution.solution
        .sliding(2)
        .map(x => Edge(x.head, x.last))
        .foreach(edge =>
          pheromone.updateWith(edge)(pheromones =>
            pheromones.map(_.map(_ + increment))
          )
        )
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
