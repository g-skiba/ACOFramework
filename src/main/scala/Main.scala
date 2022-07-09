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
import project.config.PheromoneConfig.PheromoneType
import project.config.{AlgorithmConfig, PheromoneConfig, ProblemConfig, TwoDimPheromoneConfig}
import project.logging.AcoLogger

import java.io.{File, FileInputStream, PrintWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}
import java.util.Date
import java.util.concurrent.{Executors, TimeUnit}
import scala.beans.BeanProperty
import scala.concurrent.duration.{Duration, DurationInt}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.{Random, Try}

object Main {
  val formatter: DateTimeFormatter = DateTimeFormatter
    .ofPattern("yyyy-MM-dd'T'HH-mm-ss")
    .withZone(ZoneId.systemDefault())
  val timestampStr: String = formatter.format(Instant.now())
  val outDirectory = new File(Paths.get("logs", timestampStr).toUri)
  outDirectory.mkdirs()
  val outConfFile = new File(outDirectory, "config.yaml")

  val enableLogsBuffering = true
  val sumoCollectorUrl: Option[String] =
    None

  def main(args: Array[String]): Unit = {
    val filename = "config.yaml"
    val input = new String(
      getClass.getResourceAsStream(filename).readAllBytes,
      StandardCharsets.UTF_8
    )
    writeConfFile(input)
    val yaml = new Yaml(new Constructor(classOf[ProblemConfig]))
    val conf = yaml.load[ProblemConfig](input)
    runConfiguration(conf)
  }

  def writeConfFile(config: String): Unit = {
    outConfFile.createNewFile()
    val pw = new PrintWriter(outConfFile)
    try {
      pw.write(config)
    } finally {
      pw.close()
    }
  }

  def createLogger(runId: String, metadata: Map[String, String]): AcoLogger = {
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
        val sumo = new AcoLogger.SumoLogic(runId, sumoCollectorUrl, metadata)
        new AcoLogger.MultiLogger(Seq(fileLogger, sumo))
    }
  }

  def runAlgorithm(
    baseAlgorithm: BaseAlgorithm,
    config: ProblemConfig
  ): Unit = {
    val prefix = s"${config.problemType}_${new Date().getTime.toHexString}_${Random.alphanumeric.take(5).mkString}"
    for (i <- 1 to config.repeat) {
      val runId = s"${prefix}_$i"
      val logger = createLogger(runId, config.toMap)
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

  def runConfiguration(conf: ProblemConfig): Unit = {
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
      case other =>
        throw NotImplementedError(
          s"Your method $other is not implemented!"
        )
    }

  }
}
