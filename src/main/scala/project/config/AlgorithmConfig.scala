package project.config

import scala.beans.BeanProperty

case class AlgorithmConfig(
  @BeanProperty var antsNum: Int,
  @BeanProperty var iterations: Int,
  @BeanProperty var alpha: Double,
  @BeanProperty var beta: Double,
  @BeanProperty var pheromoneConfig: PheromoneConfig
) {
  def this() = {
    this(100, 200, 2.0, 3.0, new PheromoneConfig())
  }

  def debug: Boolean = antsNum < 30 && iterations < 30
  
  override def toString: String = {
    s"antsNum: $antsNum; iterations: $iterations; alpha: $alpha; beta: $beta; $pheromoneConfig"
  }
  
  def toCsv: String = {
    Seq(antsNum, iterations, alpha, beta).mkString("", ";", ";") + pheromoneConfig.toCsv
  }

  def toMap: Map[String, String] = {
    Map(
      "antsNum" -> antsNum.toString,
      "iterations" -> iterations.toString,
      "alpha" -> alpha.toString,
      "beta" -> beta.toString,
    ) ++ pheromoneConfig.toMap
  }
}
