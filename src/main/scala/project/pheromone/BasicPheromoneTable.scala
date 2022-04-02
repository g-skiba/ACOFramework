package project.pheromone

import project.graph.{Edge, Node}
import project.solution.BaseSolution
import scala.collection.mutable.{Map => MMap}

//wiele macierzy feromonów nie ma sensu przy takiej implementacji updatu feromonów
class BasicPheromoneTable(
  edges: List[Edge],
  val increment: Double,
  val extinction: Double,
  val pheromoneDimension: Int,
  minValue: Double = 0.001,
  maxValue: Double = 0.999
) extends BasePheromoneTable {
  val pheromone: List[MMap[Edge, Double]] =
    List.fill(pheromoneDimension)(edges.map((_, maxValue)).to(MMap))
  override def getPheromone(edge: Edge): List[Double] = pheromone.map(_(edge))

  override def pheromoneUpdate(solutions: List[BaseSolution]): Unit = {
    solutions.foreach { solution =>
      solution.solution
        .sliding(2)
        .map(x => Edge(x.head, x.last))
        .foreach(edge =>
          pheromone.foreach(matrix =>
            matrix.updateWith(edge)(value => value.map(_ + increment))
          )
        )
    }
  }

  override def afterUpdatesAction(): Unit = {
    def extinctAndEnsureMinMax(double: Double): Double = {
      val e = double * (1 - extinction)
      e.min(maxValue).max(minValue)
    }
    pheromone.foreach(_.mapValuesInPlace((_, v) => extinctAndEnsureMinMax(v)))
  }

}
