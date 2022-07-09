package project.config

import scala.beans.BeanProperty

class PheromoneConfig {
  @BeanProperty var tpe: String = _
  @BeanProperty var pheromoneDimension: Int = _
  @BeanProperty var increment: Double = _
  @BeanProperty var extinction: Double = _
  @BeanProperty var minValue: Double = _
  @BeanProperty var maxValue: Double = _
  @BeanProperty var takenAntsToPheromoneUpdate: Int = _
  @BeanProperty var twoDimConfig: TwoDimPheromoneConfig = _

  def resolvePheromoneType: PheromoneConfig.PheromoneType =
    PheromoneConfig.PheromoneType.valueOf(tpe)

  def resolveTakenAntsToPheromoneUpdate: Option[Int] =
    Option(takenAntsToPheromoneUpdate).filter(_ > 0)

  def resolvePheromoneDimension: Option[Int] =
    Option(pheromoneDimension).filter(_ > 0)
}

object PheromoneConfig {
  enum PheromoneType {
    case Basic, TwoDim
  }
}
