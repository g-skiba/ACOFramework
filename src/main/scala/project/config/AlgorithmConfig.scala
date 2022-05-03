package project.config

import scala.beans.BeanProperty

class AlgorithmConfig {
  @BeanProperty var antsNum: Int = _
  @BeanProperty var iterations: Int = _
  @BeanProperty var alpha: Double = _
  @BeanProperty var beta: Double = _
  @BeanProperty var pheromoneConfig: PheromoneConfig = _
}