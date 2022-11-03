import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.jdk.CollectionConverters._
import tsp.TspReader

import vrp.VrpReader
import vrp.VrpToProblem

import java.io.File
import scala.beans.BeanProperty
import java.io.FileInputStream
import tsp.TspReader
import tsp.TspsToMtsp
import tsp.TspToProblem
import tsp.Tsp
import project.algorithm.BasicAlgorithm
import project.algorithm.SingleObjectiveSolver
import project.algorithm.BaseAlgorithm
import pareto.getParetoFrontMin
import project.config.ProblemConfig

import java.io.{File, FileInputStream, PrintWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}
import java.util.concurrent.TimeUnit
import scala.beans.BeanProperty
import scala.io.Source
import scala.jdk.CollectionConverters.*
import collection.convert.ImplicitConversions.`iterable AsScalaIterable`

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

    def runAlgorithm(baseAlgorithm: BaseAlgorithm, repeat: Int, fileName: String = "mtsp"): Unit = {
      val outResultsFile = new File(Paths.get("logs", timestampStr, s"results_$fileName.csv").toUri)
      val resultsWriter = new PrintWriter(outResultsFile)
      for(i <- 1 to repeat) {
        try {
          val start = System.nanoTime()
          baseAlgorithm.run(resultsWriter)
          val end = System.nanoTime()
          println(s"Run took: ${TimeUnit.NANOSECONDS.toMillis(end - start)}ms")
          resultsWriter.println(s"Run took: ${TimeUnit.NANOSECONDS.toMillis(end - start)}ms")
        } finally {
          // resultsWriter.close()
        }
      }
      resultsWriter.close()
    }

    val filename = "config.yaml"
    val input = new String(getClass.getResourceAsStream(filename).readAllBytes, StandardCharsets.UTF_8)
    writeConfFile(input)
    val yaml = new Yaml(new Constructor(classOf[ProblemConfig]))
    val conf = yaml.load[ProblemConfig](input)
    conf.problemType match {
      case "tsp" =>
        for ((problemFile) <- conf.problemFiles)
        {
          var tsp = TspReader.read(Source.fromResource(problemFile))
          var fileName = problemFile.split("//").apply(2)
          var (reverseNameMap, tspProblem) = TspToProblem(tsp)
          var algo = SingleObjectiveSolver(tspProblem, conf.algorithmConfig)
          runAlgorithm(algo, conf.repeat, fileName)
        }
      case "mtsp" =>
        val tsps = for {
          file <- conf.problemFiles.asScala
        } yield {
          TspReader.read(Source.fromResource(file))
        }
        val (reverseNameMap, mtspProblem) = TspsToMtsp(tsps)
        val algo = BasicAlgorithm(mtspProblem, conf.algorithmConfig)
        runAlgorithm(algo, conf.repeat)
      case "cvrp" =>
        for ((problemFile) <- conf.problemFiles)
        {
          var vrp = VrpReader.read(Source.fromResource(problemFile))
          var fileName = problemFile.split("//").apply(2)
          var (reverseNameMap, vrpProblem) = VrpToProblem(vrp)
          var algo = SingleObjectiveSolver(vrpProblem, conf.algorithmConfig)
          runAlgorithm(algo, conf.repeat, fileName)
        }
      case _ =>
        throw NotImplementedError(
          s"Your method from $filename is not implemented!"
        )
    }
  }
}