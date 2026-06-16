package mokp

import project.graph.Node
import project.problem.MokpProblem

import scala.io.Source

case class MultiObjectiveKnapsackData(
    objectives: IndexedSeq[IndexedSeq[Int]],
    constraints: IndexedSeq[IndexedSeq[Int]],
    capacities: IndexedSeq[Int]
) {
  val objectivesCount: Int = objectives.size
  val constraintsCount: Int = constraints.size
  val itemsCount: Int = objectives.headOption.map(_.size).getOrElse(0)

  require(objectivesCount == 2, s"Only 2-objective knapsack instances are supported, got $objectivesCount")
  require(itemsCount > 0, "Knapsack instance must contain at least one item")
  require(objectives.forall(_.size == itemsCount), "All objective rows must have one value per item")
  require(constraints.nonEmpty, "Knapsack instance must contain at least one constraint")
  require(constraints.forall(_.size == itemsCount), "All constraint rows must have one value per item")
  require(capacities.size == constraintsCount, "There must be one capacity per constraint")
  require(objectives.flatten.forall(_ >= 0), "Objective values must be non-negative")
  require(constraints.flatten.forall(_ >= 0), "Constraint values must be non-negative")
  require(capacities.forall(_ >= 0), "Capacities must be non-negative")
}

object MokpParser {
  def read(source: Source): MultiObjectiveKnapsackData = {
    try {
      val tokens = source.getLines()
        .flatMap(line => line.takeWhile(_ != '#').trim.split("\\s+"))
        .filter(_.nonEmpty)
        .map(_.toInt)
        .toVector

      val it = tokens.iterator

      def nextToken(label: String): Int = {
        if (!it.hasNext) throw new IllegalArgumentException(s"Missing $label in MOKP instance")
        it.next()
      }

      val itemsCount = nextToken("items count")
      val objectivesCount = nextToken("objectives count")
      val constraintsCount = nextToken("constraints count")

      val objectives = Vector.tabulate(objectivesCount) { objective =>
        Vector.tabulate(itemsCount)(_ => nextToken(s"objective ${objective + 1} value"))
      }
      val constraints = Vector.tabulate(constraintsCount) { constraint =>
        Vector.tabulate(itemsCount)(_ => nextToken(s"constraint ${constraint + 1} value"))
      }
      val capacities = Vector.tabulate(constraintsCount)(constraint => nextToken(s"capacity ${constraint + 1}"))

      if (it.hasNext) throw new IllegalArgumentException("Unexpected trailing values in MOKP instance")

      MultiObjectiveKnapsackData(objectives, constraints, capacities)
    } finally {
      source.close()
    }
  }
}
