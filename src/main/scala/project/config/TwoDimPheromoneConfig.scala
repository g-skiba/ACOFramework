package project.config

import scala.beans.BeanProperty

class TwoDimPheromoneConfig {
  @BeanProperty var size: Int = _
  @BeanProperty var getType: String = _
  @BeanProperty var updateType: String = _

  def resolveGetType: TwoDimPheromoneConfig.GetType =
    TwoDimPheromoneConfig.GetType.valueOf(getType)

  def resolveUpdateType: TwoDimPheromoneConfig.UpdateType =
    TwoDimPheromoneConfig.UpdateType.valueOf(updateType)
}

object TwoDimPheromoneConfig {
  enum GetType {
    case ExponentialRandom, WeightedCombination, PairingCombination
  }
  enum UpdateType {
    case PartFromEvaluation, PartFromIndex
  }
}
