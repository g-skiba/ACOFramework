package project.config

import project.config.PheromoneConfig.PheromoneType
import project.repo.BaseSolutionRepo
import project.solution.BaseSolution

import scala.beans.BeanProperty

case class PheromoneConfig(
  @BeanProperty var pheromoneTpe: String,
  @BeanProperty var pheromoneDimension: Int,
  @BeanProperty var increment: Double,
  @BeanProperty var extinction: Double,
  @BeanProperty var minValue: Double,
  @BeanProperty var maxValue: Double,
  @BeanProperty var solutionsSelectionStrategy: String,
  @BeanProperty var takenAntsToPheromoneUpdate: Int,
  @BeanProperty var twoDimConfig: TwoDimPheromoneConfig
) {
  def this() = {
    this(
      PheromoneType.Basic.toString, 1, 0.1, 0.1, 0.001, 0.999, SolutionsSelectionStrategy.LastIterationAll.toString, 1,
      new TwoDimPheromoneConfig()
    )
  }

  def resolvePheromoneType: PheromoneConfig.PheromoneType =
    PheromoneConfig.PheromoneType.valueOf(pheromoneTpe)

  def resolveTakenAntsToPheromoneUpdate: Option[Int] =
    Option(takenAntsToPheromoneUpdate).filter(_ > 0)

  def resolvePheromoneDimension: Option[Int] =
    Option(pheromoneDimension).filter(_ > 0)

  def resolveSolutionsSelectionStrategy: SolutionsSelectionStrategy = {
    SolutionsSelectionStrategy.valueOf(solutionsSelectionStrategy)
  }

  override def toString: String = {
    s"pheromoneTpe: $pheromoneTpe; pheromoneDimension: $pheromoneDimension; increment: $increment; extinction: $extinction; " +
      s"minValue: $minValue; maxValue: $maxValue; solutionsSelectionStrategy: $solutionsSelectionStrategy, " +
      s"takenAntsToPheromoneUpdate: $takenAntsToPheromoneUpdate; $twoDimConfig"
  }

  def toCsv: String = {
    Seq(pheromoneTpe, pheromoneDimension, increment, extinction, minValue, maxValue, solutionsSelectionStrategy, takenAntsToPheromoneUpdate).mkString("", ";", ";") +
      twoDimConfig.toCsv
  }

  def toMap: Map[String, String] = {
    Map(
      "pheromoneTpe" -> pheromoneTpe,
      "pheromoneDimension" -> pheromoneDimension.toString,
      "increment" -> increment.toString,
      "extinction" -> extinction.toString,
      "minValue" -> minValue.toString,
      "maxValue" -> maxValue.toString,
      "solutionsSelectionStrategy" -> solutionsSelectionStrategy,
      "takenAntsToPheromoneUpdate" -> takenAntsToPheromoneUpdate.toString,
    ) ++ twoDimConfig.toMap
  }
}

object PheromoneConfig {
  enum PheromoneType {
    case Basic, TwoDim
  }
}

enum SolutionsSelectionStrategy(fun: BaseSolutionRepo => IndexedSeq[BaseSolution]) {
  def apply(solutionsRepo: BaseSolutionRepo): IndexedSeq[BaseSolution] = fun(solutionsRepo)

  case LastIterationAll extends SolutionsSelectionStrategy(_.solutionsForLastIteration)
  case LastIterationPareto extends SolutionsSelectionStrategy(_.paretoSolutionsForLastIteration)
  case GlobalPareto extends SolutionsSelectionStrategy(_.globalParetoSolutions)
}