import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor
import pareto.getParetoFrontMin
import project.algorithm.{BaseAlgorithm, BasicAlgorithm, SingleObjectiveSolver}
import project.config.ProblemConfig
import tsp.{Tsp, TspReader, TspToProblem, TspsToMtsp}

import java.io.{File, FileInputStream}
import scala.beans.BeanProperty
import scala.io.Source
import scala.jdk.CollectionConverters.*

object Main {
  def main(args: Array[String]): Unit = {
    val filename = "config.yaml"
    val input = getClass.getResourceAsStream(filename)
    val yaml = new Yaml(new Constructor(classOf[ProblemConfig]))
    val conf = yaml.load[ProblemConfig](input)
    conf.problemType match {
      case "tsp" =>
        val tsp = TspReader.read(Source.fromResource(conf.problemFiles.get(0)))
        val (reverseNameMap, tspProblem) = TspToProblem(tsp)
        val algo = SingleObjectiveSolver(tspProblem, conf.algorithmConfig)
        algo.run()
      case "mtsp" =>
        val tsps = for {
          file <- conf.problemFiles.asScala
        } yield {
          TspReader.read(Source.fromResource(file))
        }
        val (reverseNameMap, mtspProblem) = TspsToMtsp(tsps)
        val algo = BasicAlgorithm(mtspProblem, conf.algorithmConfig)
        algo.run()
      case _ =>
        throw NotImplementedError(
          s"Your method from $filename is not implemented!"
        )
    }

  }
}