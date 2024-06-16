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

  def toCsv: String = {
    Seq(twoDimSize, twoDimGetType, twoDimUpdateType).mkString(";")
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
  enum GetType(val isRandomized: Boolean) {
    case ExponentialRandom extends GetType(true)
    case ExponentialRandomMax extends GetType(true)
    case WeightedCombination extends GetType(false)
    case PairingCombination extends GetType(false)
    case ExpectedCombination extends GetType(false)
  }
  enum UpdateType {
    case PartFromEvaluation, PartFromIndex
  }
}
