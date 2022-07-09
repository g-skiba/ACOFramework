package project.config

import scala.beans.BeanProperty

case class ProblemConfig(
  @BeanProperty var problemType: String,
  @BeanProperty var problemFiles: java.util.List[String],
  @BeanProperty var repeat: Int,
  @BeanProperty var algorithmConfig: AlgorithmConfig
) {
  def this() = {
    this("", new java.util.ArrayList[String](), 100, new AlgorithmConfig())
  }

  override def toString: String = {
    s"problemType: $problemType; problemFiles: ${String.join(",", problemFiles)}; " +
      s"repeat: $repeat; $algorithmConfig"
  }

  def toMap: Map[String, String] = {
    Map(
      "problemType" -> problemType,
      "problemFiles" -> String.join("&", problemFiles),
      "repeat" -> repeat.toString,
    ) ++ algorithmConfig.toMap
  }
}
