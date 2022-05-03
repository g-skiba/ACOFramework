package project.config

import scala.beans.BeanProperty

class ProblemConfig {
  @BeanProperty var problemType: String = _
  @BeanProperty var problemFiles = new java.util.ArrayList[String]()
  @BeanProperty var algorithmConfig: AlgorithmConfig = _
  override def toString: String = {
    String.join(",", problemFiles)
  }
}
