import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.jdk.CollectionConverters.*
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
import project.logging.AcoLogger

import java.io.{File, FileInputStream, PrintWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}
import java.util.Date
import java.util.concurrent.TimeUnit
import scala.beans.BeanProperty
import scala.io.Source
import scala.jdk.CollectionConverters.*

object Main {
  def main(args: Array[String]): Unit = {
    val formatter = DateTimeFormatter
      .ofPattern("yyyy-MM-dd'T'HH-mm-ss")
      .withZone(ZoneId.systemDefault())
    val timestampStr = formatter.format(Instant.now())
    val outDirectory = new File(Paths.get("logs", timestampStr).toUri)
    outDirectory.mkdirs()
    val outConfFile = new File(outDirectory, "config.yaml")

    val enableLogsBuffering = false
    val sumoCollectorUrl: Option[String] =
      None // add collector URL to send logs to Sumo Logic

    def writeConfFile(config: String): Unit = {
      outConfFile.createNewFile()
      val pw = new PrintWriter(outConfFile)
      try {
        pw.write(config)
      } finally {
        pw.close()
      }
    }

    def createLogger(runId: String): AcoLogger = {
      val outResultsFile = new File(
        Paths.get("logs", timestampStr, s"results_$runId.csv").toUri
      )
      val resultsWriter = new PrintWriter(outResultsFile)
      val fileLogger = {
        if (!enableLogsBuffering)
          new AcoLogger.StdOutAndFile(runId, resultsWriter)
        else
          new AcoLogger.StdOutAndFileBuffering(runId, resultsWriter)
      }
      sumoCollectorUrl match {
        case None => fileLogger
        case Some(sumoCollectorUrl) =>
          val sumo = new AcoLogger.SumoLogic(runId, sumoCollectorUrl)
          new AcoLogger.MultiLogger(Seq(fileLogger, sumo))
      }
    }

    def runAlgorithm(
        baseAlgorithm: BaseAlgorithm,
        config: ProblemConfig
    ): Unit = {
      val runId = s"${config.problemType}_${new Date().getTime}"
      for (_ <- 1 to config.repeat) {
        val logger = createLogger(runId)
        try {
          logger.config(config)

          val start = System.nanoTime()
          val result = baseAlgorithm.run(logger)
          val end = System.nanoTime()

          logger.globalBestResult(result.globalBest)

          logger.runTimeInfo(end - start)
        } finally {
          logger.close()
        }
      }
    }

    val filename = "config.yaml"
    val input = new String(
      getClass.getResourceAsStream(filename).readAllBytes,
      StandardCharsets.UTF_8
    )
    writeConfFile(input)
    val yaml = new Yaml(new Constructor(classOf[ProblemConfig]))
    val conf = yaml.load[ProblemConfig](input)
    conf.problemType match {
      case "tsp" =>
        val tsp = TspReader.read(Source.fromResource(conf.problemFiles.get(0)))
        val (reverseNameMap, tspProblem) = TspToProblem(tsp)
        val algo = SingleObjectiveSolver(tspProblem, conf.algorithmConfig)
        runAlgorithm(algo, conf)
      case "mtsp" =>
        val tsps = for {
          file <- conf.problemFiles.asScala
        } yield {
          TspReader.read(Source.fromResource(file))
        }
        val (reverseNameMap, mtspProblem) = TspsToMtsp(tsps)
        val algo = BasicAlgorithm(mtspProblem, conf.algorithmConfig)
        runAlgorithm(algo, conf)
      case "cvrp" =>
        val vrp = VrpReader.read(Source.fromResource(conf.problemFiles.get(0)))
        val (reverseNameMap, vrpProblem) = VrpToProblem(vrp)
        val algo = SingleObjectiveSolver(vrpProblem, conf.algorithmConfig)
        runAlgorithm(algo, conf)
      case _ =>
        throw NotImplementedError(
          s"Your method from $filename is not implemented!"
        )
    }

  }
}
