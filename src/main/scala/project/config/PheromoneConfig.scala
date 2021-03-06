package project.config

import scala.beans.BeanProperty

case class PheromoneConfig(
  @BeanProperty var pheromoneTpe: String,
  @BeanProperty var pheromoneDimension: Int,
  @BeanProperty var increment: Double,
  @BeanProperty var extinction: Double,
  @BeanProperty var minValue: Double,
  @BeanProperty var maxValue: Double,
  @BeanProperty var takenAntsToPheromoneUpdate: Int,
  @BeanProperty var twoDimConfig: TwoDimPheromoneConfig
) {
  def this() = {
    this("Basic", 1, 0.1, 0.1, 0.001, 0.999, 1, new TwoDimPheromoneConfig())
  }

  def resolvePheromoneType: PheromoneConfig.PheromoneType =
    PheromoneConfig.PheromoneType.valueOf(pheromoneTpe)

  def resolveTakenAntsToPheromoneUpdate: Option[Int] =
    Option(takenAntsToPheromoneUpdate).filter(_ > 0)

  def resolvePheromoneDimension: Option[Int] =
    Option(pheromoneDimension).filter(_ > 0)

  override def toString: String = {
    s"pheromoneTpe: $pheromoneTpe; pheromoneDimension: $pheromoneDimension; increment: $increment; extinction: $extinction; " +
      s"minValue: $minValue; maxValue: $maxValue; takenAntsToPheromoneUpdate: $takenAntsToPheromoneUpdate; $twoDimConfig"
  }

  def toMap: Map[String, String] = {
    Map(
      "pheromoneTpe" -> pheromoneTpe,
      "pheromoneDimension" -> pheromoneDimension.toString,
      "increment" -> increment.toString,
      "extinction" -> extinction.toString,
      "minValue" -> minValue.toString,
      "maxValue" -> maxValue.toString,
      "takenAntsToPheromoneUpdate" -> takenAntsToPheromoneUpdate.toString,
    ) ++ twoDimConfig.toMap
  }
}

object PheromoneConfig {
  enum PheromoneType {
    case Basic, TwoDim
  }
}
