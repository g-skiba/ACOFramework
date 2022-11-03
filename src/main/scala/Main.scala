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
import collection.convert.ImplicitConversions.`iterable AsScalaIterable`
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
  val writeToStdOut = false
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
        new AcoLogger.StdOutAndFile(runId, writeToStdOut, resultsWriter)
      else
        new AcoLogger.StdOutAndFileBuffering(runId, writeToStdOut, resultsWriter)
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
    config: ProblemConfig,
    fileName: String = "mtsp"
  ): Unit = {
    val prefix = s"${config.problemType}_${new Date().getTime.toHexString}_${Random.alphanumeric.take(5).mkString}_$fileName"
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
        for ((problemFile) <- conf.problemFiles)
        {
          var tsp = TspReader.read(Source.fromResource(problemFile))
          var fileName = problemFile.split("//").apply(2)
          var (reverseNameMap, tspProblem) = TspToProblem(tsp)
          var algo = SingleObjectiveSolver(tspProblem, conf.algorithmConfig)
          runAlgorithm(algo, conf, fileName)
        }
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
        for ((problemFile) <- conf.problemFiles)
        {
          var vrp = VrpReader.read(Source.fromResource(problemFile))
          var fileName = problemFile.split("//").apply(2)
          var (reverseNameMap, vrpProblem) = VrpToProblem(vrp)
          var algo = SingleObjectiveSolver(vrpProblem, conf.algorithmConfig)
          runAlgorithm(algo, conf, fileName)
        }
      case other =>
        throw NotImplementedError(
          s"Your method $other is not implemented!"
        )
    }
  }
}

object RunLoop {
  def main(args: Array[String]): Unit = {
    val problemType = "tsp"
    val repeat = 10

    //ProblemConfig
    val problemInstances = List(
      "mtsp/berlin52.tsp",
      "mtsp/lust_kroA100.tsp",
      "mtsp/tsp225.tsp",
      "mtsp/a280mod.tsp",
      "mtsp/pcb442.tsp",
//      "mtsp/rat575.tsp"
    )

    //AlgorithmConfig
    val antsNums = List(
      20,
      50,
      100
    )
    val iterationsNums = List(
      150
    )
    val alphas = List(
      2.0,
      3.0
    )
    val betas = List(
      2.0,
      3.0
    )

    //PheromoneConfig
    val tpes = List(
      PheromoneConfig.PheromoneType.Basic,
      PheromoneConfig.PheromoneType.TwoDim
    )
    val dimensions = List(1)
    val incrementsAndExtinctions = List(
      (0.01, 0.01),
      (0.05, 0.05),
      (0.1, 0.1),
    )
    val minValues = List(0.001)
    val maxValues = List(0.999)
    val takenAntsToPheromoneUpdates = (for {
      tpe <- tpes
      antsNum <- antsNums
    } yield {
      val values = tpe match {
        case PheromoneType.Basic =>
          List(1, antsNum / 2, -1)
//          List(1)
        case PheromoneType.TwoDim =>
          List(antsNum / 2, -1)
//          List(-1)
      }
      (tpe, antsNum) -> values
    }).toMap

    //TwoDimPheromoneConfig
    val twoDimSizes = List(
      4,
      10,
      20
    )
    val getTypes = List(
      TwoDimPheromoneConfig.GetType.ExponentialRandom,
      TwoDimPheromoneConfig.GetType.PairingCombination,
      TwoDimPheromoneConfig.GetType.WeightedCombination,
    )
    val updateTypes = List(
      TwoDimPheromoneConfig.UpdateType.PartFromIndex,
      TwoDimPheromoneConfig.UpdateType.PartFromEvaluation,
    )

    val ec: ExecutionContext = ExecutionContextHelper.fixed("worker", size = 12)
    val futures = for {
      problemInstance <- problemInstances
      antsNum <- antsNums
      iterations <- iterationsNums
      alpha <- alphas
      beta <- betas
      phTpe <- tpes
      phDimension <- dimensions
      (phIncrement, phExtinction) <- incrementsAndExtinctions
      phMinValue <- minValues
      phMaxValue <- maxValues
      takenAntsToPheromoneUpdate <- takenAntsToPheromoneUpdates(phTpe, antsNum)
      twoDimConfig <- {
        phTpe match {
          case PheromoneConfig.PheromoneType.Basic => List(new TwoDimPheromoneConfig()) //irrelevant
          case PheromoneConfig.PheromoneType.TwoDim =>
            for {
              size <- twoDimSizes
              getType <- getTypes
              updateType <- updateTypes
            } yield {
              new TwoDimPheromoneConfig(size, getType.toString, updateType.toString)
            }
        }
      }
    } yield {
      val pheromoneConfig = PheromoneConfig(
        phTpe.toString, phDimension, phIncrement, phExtinction, phMinValue, phMaxValue, takenAntsToPheromoneUpdate,
        twoDimConfig
      )
      val algorithmConfig = AlgorithmConfig(antsNum, iterations, alpha, beta, pheromoneConfig)
      val problemConfig = ProblemConfig(problemType, List(problemInstance).asJava, repeat, algorithmConfig)

//      problemConfig
      Future {
        Try(Main.runConfiguration(problemConfig)).failed.foreach { e =>
          println(problemConfig)
          e.printStackTrace()
        }
      }(ec)
    }

//    println(futures.size)
    {
      import ExecutionContext.Implicits.global
      Await.result(Future.sequence(futures), Duration.Inf)
    }
  }
}
