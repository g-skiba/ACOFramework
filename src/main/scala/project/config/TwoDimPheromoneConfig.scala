package project.config

import scala.beans.BeanProperty

case class TwoDimPheromoneConfig(
  @BeanProperty var twoDimSize: Int,
  @BeanProperty var twoDimGetType: String,
  @BeanProperty var twoDimUpdateType: String
) {
  def this() = {
    this(10, "ExponentialRandom", "PartFromEvaluation")
  }

  def resolveGetType: TwoDimPheromoneConfig.GetType =
    TwoDimPheromoneConfig.GetType.valueOf(twoDimGetType)

  def resolveUpdateType: TwoDimPheromoneConfig.UpdateType =
    TwoDimPheromoneConfig.UpdateType.valueOf(twoDimUpdateType)

  override def toString: String = {
    s"twoDimSize: $twoDimSize; twoDimGetType: $twoDimGetType; twoDimUpdateType: $twoDimUpdateType"
  }

  def toMap: Map[String, String] = {
    Map(
      "twoDimSize" -> twoDimSize.toString,
      "twoDimGetType" -> twoDimGetType,
      "twoDimUpdateType" -> twoDimUpdateType
    )
  }
}

object TwoDimPheromoneConfig {
  enum GetType {
    case ExponentialRandom, ExponentialRandomMax, WeightedCombination, PairingCombination, ExpectedCombination
  }
  enum UpdateType {
    case PartFromEvaluation, PartFromIndex
  }
}
