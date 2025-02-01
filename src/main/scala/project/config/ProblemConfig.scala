package project.config

import scala.beans.BeanProperty
import scala.jdk.CollectionConverters._

case class ProblemConfig(
  @BeanProperty var problemType: String,
  @BeanProperty var problemFiles: java.util.List[String],
  @BeanProperty var repeats: Int,
  @BeanProperty var algorithmConfig: AlgorithmConfig
) {
  def this() = {
    this("", new java.util.ArrayList[String](), 100, new AlgorithmConfig())
  }

  def debug: Boolean = algorithmConfig.debug

  override def toString: String = {
    s"problemType: $problemType; problemFiles: ${String.join(",", problemFiles)}; " +
      s"repeats: $repeats; $algorithmConfig"
  }

  def toCsv: String = {
    Seq(problemType, problemFiles.asScala.mkString(","), repeats).mkString("", ";", ";") + algorithmConfig.toCsv
  }

  def toMap: Map[String, String] = {
    Map(
      "problemType" -> problemType,
      "problemFiles" -> String.join("&", problemFiles),
      "repeats" -> repeats.toString,
    ) ++ algorithmConfig.toMap
  }
}
