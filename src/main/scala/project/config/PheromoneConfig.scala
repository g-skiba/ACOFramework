package project.config

import scala.beans.BeanProperty

class PheromoneConfig {
  @BeanProperty var tpe: String = _ //Basic/TwoDim
  @BeanProperty var increment: Double = _
  @BeanProperty var extinction: Double = _
}
