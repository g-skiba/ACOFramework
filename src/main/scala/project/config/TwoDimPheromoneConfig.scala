package project.config

import scala.beans.BeanProperty

case class TwoDimPheromoneConfig(
    @BeanProperty var size: Int,
    @BeanProperty var getType: String,
    @BeanProperty var updateType: String
) {
  def this() = {
    this(5, "ExponentialRandom", "PartFromEvaluation")
  }

  def resolveGetType: TwoDimPheromoneConfig.GetType =
    TwoDimPheromoneConfig.GetType.valueOf(getType)

  def resolveUpdateType: TwoDimPheromoneConfig.UpdateType =
    TwoDimPheromoneConfig.UpdateType.valueOf(updateType)

  override def toString: String = {
    s"size: $size; getType: $getType; updateType: $updateType"
  }
}

object TwoDimPheromoneConfig {
  enum GetType {
    case ExponentialRandom, WeightedCombination, PairingCombination
  }
  enum UpdateType {
    case PartFromEvaluation, PartFromIndex
  }
}
