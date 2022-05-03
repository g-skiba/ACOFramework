package project.config

import scala.beans.BeanProperty

class AlgorithmConfig {
  @BeanProperty var antsNum: Int = _
  @BeanProperty var iterations: Int = _
}