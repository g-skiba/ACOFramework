import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.jdk.CollectionConverters._
import tsp.TspReader

import java.io.File
import scala.beans.BeanProperty
import java.io.FileInputStream
import tsp.TspReader
import tsp.TspsToMtsp
import tsp.TspToProblem
import tsp.Tsp
import project.algorithm.BasicAlgorithm
import project.algorithm.TspSolver
import project.algorithm.BaseAlgorithm
import pareto.getParetoFrontMin

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val filename = "config.yaml"
    val input = getClass.getResourceAsStream(filename)
    val yaml = new Yaml(new Constructor(classOf[ProblemConfig]))
    val antsNumber = 100
    val algorithmIterations = 100
    val conf = yaml.load[ProblemConfig](input)
    conf.problemType match {
      case "tsp" =>
        val tsp = TspReader.read(Source.fromResource(conf.problemFiles.get(0)))
        val (reverseNameMap, tspProblem) = TspToProblem(tsp)
        val algo = TspSolver(antsNumber, tspProblem, algorithmIterations)
        algo.run()
      case "mtsp" =>
        val tsps = for {
          file <- conf.problemFiles.asScala
        } yield {
          TspReader.read(Source.fromResource(file))
        }
        val (reverseNameMap, mtspProblem) = TspsToMtsp(tsps)
        val algo =
          BasicAlgorithm(antsNumber, mtspProblem, algorithmIterations)
        algo.run()
      case _ =>
        throw NotImplementedError(
          s"Your method from $filename is not implemented!"
        )
    }

  }
}
class ProblemConfig {
  @BeanProperty var problemType: String = _
  @BeanProperty var problemFiles = new java.util.ArrayList[String]()
  override def toString: String = {
    String.join(",", problemFiles)
  }
}
