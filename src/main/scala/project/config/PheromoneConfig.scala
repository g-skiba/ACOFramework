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
  @BeanProperty var twoDimConfig: TwoDimPheromoneConfig,
  @BeanProperty var paretoFrontsCount: Int = 0,
  @BeanProperty var paretoHistoryLength: Int = 0
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

  def validateLearningArchive(): Unit = {
    require(paretoFrontsCount >= 0, "paretoFrontsCount must be nonnegative; zero disables the learning archive")
    require(paretoHistoryLength >= 0, "paretoHistoryLength must be nonnegative; zero means unlimited")
    require(
      resolveSolutionsSelectionStrategy != SolutionsSelectionStrategy.ParetoFronts || paretoFrontsCount > 0,
      "ParetoFronts requires paretoFrontsCount > 0"
    )
    require(
      resolveSolutionsSelectionStrategy == SolutionsSelectionStrategy.ParetoFronts || paretoFrontsCount == 0,
      "paretoFrontsCount must be 0 unless solutionsSelectionStrategy is ParetoFronts"
    )
  }

  /** Resolve after validating that the front limit is applicable to the selection strategy. */
  def resolveParetoFrontsCount: Int = {
    validateLearningArchive()
    paretoFrontsCount
  }

  override def toString: String = {
    s"pheromoneTpe: $pheromoneTpe; pheromoneDimension: $pheromoneDimension; increment: $increment; extinction: $extinction; " +
      s"minValue: $minValue; maxValue: $maxValue; solutionsSelectionStrategy: $solutionsSelectionStrategy, " +
      s"takenAntsToPheromoneUpdate: $takenAntsToPheromoneUpdate; " +
      s"paretoFrontsCount: $paretoFrontsCount; paretoHistoryLength: $paretoHistoryLength; $twoDimConfig"
  }

  def toCsv: String = {
    Seq(pheromoneTpe, pheromoneDimension, increment, extinction, minValue, maxValue, solutionsSelectionStrategy, takenAntsToPheromoneUpdate).mkString("", ";", ";") +
      twoDimConfig.toCsv + Seq(paretoFrontsCount, paretoHistoryLength).mkString(";", ";", "")
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
      "paretoFrontsCount" -> paretoFrontsCount.toString,
      "paretoHistoryLength" -> paretoHistoryLength.toString,
    ) ++ twoDimConfig.toMap
  }
}

object PheromoneConfig {
  enum PheromoneType {
    case Basic, TwoDim, ItemBasic, ItemTwoDim
  }
}

enum SolutionsSelectionStrategy(fun: BaseSolutionRepo => IndexedSeq[IndexedSeq[BaseSolution]]) {
  def apply(solutionsRepo: BaseSolutionRepo): IndexedSeq[IndexedSeq[BaseSolution]] = fun(solutionsRepo)

  case LastIterationAll extends SolutionsSelectionStrategy(repo => IndexedSeq(repo.solutionsForLastIteration))
  case LastIterationPareto extends SolutionsSelectionStrategy(repo => IndexedSeq(repo.paretoSolutionsForLastIteration))
  case GlobalPareto extends SolutionsSelectionStrategy(repo => IndexedSeq(repo.globalParetoSolutions))
  case ParetoFronts extends SolutionsSelectionStrategy(_.learningParetoFronts)
}
