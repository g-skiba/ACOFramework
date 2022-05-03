import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor
import pareto.getParetoFrontMin
import project.algorithm.{BaseAlgorithm, BasicAlgorithm, SingleObjectiveSolver}
import project.config.ProblemConfig
import tsp.{Tsp, TspReader, TspToProblem, TspsToMtsp}

import java.io.{File, FileInputStream, PrintWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter
import scala.beans.BeanProperty
import scala.io.Source
import scala.jdk.CollectionConverters.*

object Main {
  def main(args: Array[String]): Unit = {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH-mm-ss").withZone(ZoneId.systemDefault())
    val timestampStr = formatter.format(Instant.now())
    val outDirectory = new File(Paths.get("logs", timestampStr).toUri)
    outDirectory.mkdirs()
    val outConfFile = new File(outDirectory, "config.yaml")
    def writeConfFile(config: String): Unit = {
      outConfFile.createNewFile()
      val pw = new PrintWriter(outConfFile)
      try {
        pw.write(config)
      } finally {
        pw.close()
      }
    }

    def runAlgorithm(baseAlgorithm: BaseAlgorithm): Unit = {
      val outResultsFile = new File(Paths.get("logs", timestampStr, "results.csv").toUri)
      val resultsWriter = new PrintWriter(outResultsFile)
      try {
        baseAlgorithm.run(resultsWriter)
      } finally {
        resultsWriter.close()
      }
    }

    val filename = "config.yaml"
    val input = new String(getClass.getResourceAsStream(filename).readAllBytes, StandardCharsets.UTF_8)
    writeConfFile(input)
    val yaml = new Yaml(new Constructor(classOf[ProblemConfig]))
    val conf = yaml.load[ProblemConfig](input)
    conf.problemType match {
      case "tsp" =>
        val tsp = TspReader.read(Source.fromResource(conf.problemFiles.get(0)))
        val (reverseNameMap, tspProblem) = TspToProblem(tsp)
        val algo = SingleObjectiveSolver(tspProblem, conf.algorithmConfig)
        runAlgorithm(algo)
      case "mtsp" =>
        val tsps = for {
          file <- conf.problemFiles.asScala
        } yield {
          TspReader.read(Source.fromResource(file))
        }
        val (reverseNameMap, mtspProblem) = TspsToMtsp(tsps)
        val algo = BasicAlgorithm(mtspProblem, conf.algorithmConfig)
        runAlgorithm(algo)
      case _ =>
        throw NotImplementedError(
          s"Your method from $filename is not implemented!"
        )
    }

  }
}