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
import project.algorithm.BaseAlgorithm
import pareto.{Hypervolume2DCalculator, getParetoFrontMin}
import project.config.PheromoneConfig.PheromoneType
import project.config.{AlgorithmConfig, PheromoneConfig, ProblemConfig, SolutionsSelectionStrategy, TwoDimPheromoneConfig}
import project.logging.{AcoLogger, DebugLogger, IraceSingleObjectiveStdOutLogger, MultiLogger, StdOutAndCsvFileBuffering2DLogger, StdOutAndCsvFileBufferingRepeatAgnosticOnlyFinalResult2DLogger, StdOutLogger, SumoLogicLogger, WarnLogger}
import project.problem.BaseProblem

import java.io.{File, FileInputStream, PrintWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
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

  val enableLogsBuffering = false
  val writeToStdOut = true
  if (!writeToStdOut) WarnLogger.disable()
  val writeToFile = false
  val writeConfigurationFile = false
  val sumoCollectorUrl: Option[String] =
    None

  def main(args: Array[String]): Unit = {
    val filename = "config.yaml"
    val input = new String(Files.readAllBytes(Paths.get(getClass.getResource(filename).toURI)))
    if (writeConfigurationFile) writeConfFile(input)
    val yaml = new Yaml(new Constructor(classOf[ProblemConfig]))
    val conf = yaml.load[ProblemConfig](input)
    if (conf.debug) DebugLogger.enable()
    runConfiguration(conf)
  }

  def writeConfFile(config: String): Unit = {
    val outDirectory = new File(Paths.get("logs", timestampStr).toUri)
    outDirectory.mkdirs()
    val outConfFile = new File(outDirectory, "config.yaml")
    outConfFile.createNewFile()
    val pw = new PrintWriter(outConfFile)
    try {
      pw.write(config)
    } finally {
      pw.close()
    }
  }

  def createLogger(runId: String, problem: BaseProblem[_], metadata: Map[String, String]): AcoLogger = {
    // can be used for loggers writing to files
    def createFileAndWriter(prefix: String): Option[PrintWriter] = {
      if (writeToFile) {
        val outResultsFile = new File(
          Paths.get("logs", timestampStr, prefix, s"${prefix}_results_$runId.csv").toUri
        )
        outResultsFile.getParentFile.mkdirs()
        Some(new PrintWriter(outResultsFile))
      } else {
        None
      }
    }

    val basicLogger = problem.dimensions match {
      case 1 => new IraceSingleObjectiveStdOutLogger
      case 2 =>
//        val resultsWriter = createFileAndWriter("results")
//        val hvCalc = problem.getHypervolumeCalculator.get
//        new StdOutAndCsvFileBufferingRepeatAgnosticOnlyFinalResult2DLogger(writeToStdOut, resultsWriter, hvCalc)
        val iterationWriter = createFileAndWriter("iteration")
        val globalWriter = createFileAndWriter("global")
        val hvCalc = problem.getHypervolumeCalculator.get
        new StdOutAndCsvFileBuffering2DLogger(writeToStdOut, iterationWriter, globalWriter, hvCalc)
      case n => new StdOutLogger(runId)
    }
    sumoCollectorUrl match {
      case None => basicLogger
      case Some(sumoCollectorUrl) =>
        val sumo = new SumoLogicLogger(runId, sumoCollectorUrl, metadata)
        new MultiLogger(Seq(basicLogger, sumo))
    }
  }

  def runAlgorithm(
    baseAlgorithm: BaseAlgorithm,
    config: ProblemConfig,
    loggerOverride: Option[AcoLogger]
  ): Unit = {
    val prefix = s"${config.problemType}_${new Date().getTime.toHexString}_${Random.alphanumeric.take(5).mkString}"
//    val logger = createLogger(prefix, baseAlgorithm.problem, config.toMap)
//    try {
    for (i <- 1 to config.repeats) {
      val runId = s"${prefix}_$i"
      val logger = loggerOverride.getOrElse(
        createLogger(runId, baseAlgorithm.problem, config.toMap)
      )
      try {
        logger.config(config, i)

        val start = System.nanoTime()
        val result = baseAlgorithm.run(logger)
        val end = System.nanoTime()

        logger.globalBestResult(result.globalParetoSolutions)

        logger.runTimeInfo(end - start)
      } finally {
        logger.close()
      }
    }
//    } finally {
//      logger.foreach(_.close())
//    }
  }

  def runConfiguration(conf: ProblemConfig, seed: Option[Long] = None, loggerOverride: Option[AcoLogger] = None): Unit = {
    conf.problemType match {
      case "tsp" =>
        val tsp = TspReader.read(Source.fromResource(conf.problemFiles.get(0)))
        val (reverseNameMap, tspProblem) = TspToProblem(tsp)
        val algo = BasicAlgorithm(tspProblem, conf.algorithmConfig, seed)
        runAlgorithm(algo, conf, loggerOverride)
      case "mtsp" =>
        val tsps = for {
          file <- conf.problemFiles.asScala
        } yield {
          TspReader.read(Source.fromResource(file))
        }
        val (reverseNameMap, mtspProblem) = TspsToMtsp(tsps)
        val algo = BasicAlgorithm(mtspProblem, conf.algorithmConfig, seed)
        runAlgorithm(algo, conf, loggerOverride)
      case "cvrp" =>
        val vrp = VrpReader.read(Source.fromResource(conf.problemFiles.get(0)))
        val (reverseNameMap, vrpProblem) = VrpToProblem(vrp)
        val algo = BasicAlgorithm(vrpProblem, conf.algorithmConfig, seed)
        runAlgorithm(algo, conf, loggerOverride)
      case other =>
        throw NotImplementedError(
          s"Your method $other is not implemented!"
        )
    }
  }
}

object RunLoop {
  def main(args: Array[String]): Unit = {
    val problemType = "mtsp"
    val repeat = 10

    //ProblemConfig
    val problemInstances = List(
//      "mtsp/berlin52.tsp",
      List("mtsp/lust_kroA100.tsp", "mtsp/lust_kroB100.tsp"),
//      "mtsp/tsp225.tsp",
//      "mtsp/a280mod.tsp",
//      "mtsp/pcb442.tsp",
//      "mtsp/rat575.tsp"
    )

    //AlgorithmConfig
    val antsNums = List(
      20,
      50,
      100
    )
    val iterationsNums = List(
      200
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
    def solutionsSelectionStrategies(pheromoneType: PheromoneType) = {
      import SolutionsSelectionStrategy._
      pheromoneType match {
        case PheromoneType.Basic => Seq(LastIterationAll, LastIterationPareto, GlobalPareto)
        case PheromoneType.TwoDim => Seq(LastIterationAll, LastIterationPareto, GlobalPareto)
      }
    }

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
      problemInstances <- problemInstances
      antsNum <- antsNums
      iterations <- iterationsNums
      alpha <- alphas
      beta <- betas
      phTpe <- tpes
      phDimension <- dimensions
      (phIncrement, phExtinction) <- incrementsAndExtinctions
      phMinValue <- minValues
      phMaxValue <- maxValues
      solutionsSelectionStrategy <- solutionsSelectionStrategies(phTpe)
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
        phTpe.toString, phDimension, phIncrement, phExtinction, phMinValue, phMaxValue,
        solutionsSelectionStrategy.toString, takenAntsToPheromoneUpdate, twoDimConfig
      )
      val algorithmConfig = AlgorithmConfig(antsNum, iterations, alpha, beta, pheromoneConfig)
      val problemConfig = ProblemConfig(problemType, problemInstances.asJava, repeat, algorithmConfig)

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

/**
 * In order to run this and get a file with final results from all repeats per config+instance, you need to modify the
 * runAlgorithm and createLogger functions to create a StdOutAndCsvFileBufferingRepeatAgnosticOnlyFinalResult2DLogger
 * once for all repeats.
 */
object TestConfigsOnInstances {
  def main(args: Array[String]): Unit = {
    val problemType = "mtsp"
    val repeat = 20

    //ProblemConfig
    val problemInstances = List(
      //      "mtsp/berlin52.tsp",
//      List("mtsp/kroA150.tsp", "mtsp/kroB150.tsp"), //"mtsp/kroC100.tsp", "mtsp/kroD100.tsp", "mtsp/kroE100.tsp"
//      List("mtsp/kroA200.tsp", "mtsp/kroB200.tsp"), //"mtsp/kroC100.tsp", "mtsp/kroD100.tsp", "mtsp/kroE100.tsp"
      List("mtsp/kroE100.tsp", "mtsp/kroF100.tsp"), //"mtsp/kroC100.tsp", "mtsp/kroD100.tsp", "mtsp/kroE100.tsp"
      List("mtsp/kroA300.tsp", "mtsp/kroB300.tsp"), //"mtsp/kroC100.tsp", "mtsp/kroD100.tsp", "mtsp/kroE100.tsp"
//      List("mtsp/paquete_euclidA100.tsp", "mtsp/paquete_euclidB100.tsp"),
      //      "mtsp/tsp225.tsp",
      //      "mtsp/a280mod.tsp",
      //      "mtsp/pcb442.tsp",
      //      "mtsp/rat575.tsp"
    )

    //AlgorithmConfig
    val antsNum = 100
    val iterationsNum = 300
    val alpha = 2.0
    val beta = 3.0

    //PheromoneConfig
    val phDimension = -1
    val minValue = 0.001
    val maxValue = 0.999

    import TwoDimPheromoneConfig._

    val config1 = {
      val twoDimPhConfig = TwoDimPheromoneConfig(
        20, GetType.ExponentialRandomMax.toString, UpdateType.PartFromEvaluation.toString
      )
      val pheromoneConfig = PheromoneConfig(
        PheromoneType.TwoDim.toString, phDimension, 0.05, 0.05, minValue, maxValue,
        SolutionsSelectionStrategy.LastIterationPareto.toString, 60, twoDimPhConfig
      )
      AlgorithmConfig(antsNum, iterationsNum, alpha, beta, pheromoneConfig)
    }
    val config2 = {
      val twoDimPhConfig = TwoDimPheromoneConfig(
        20, GetType.ExponentialRandomMax.toString, UpdateType.PartFromEvaluation.toString
      )
      val pheromoneConfig = PheromoneConfig(
        PheromoneType.TwoDim.toString, phDimension, 0.05, 0.05, minValue, maxValue,
        SolutionsSelectionStrategy.LastIterationPareto.toString, 100, twoDimPhConfig
      )
      AlgorithmConfig(antsNum, iterationsNum, alpha, beta, pheromoneConfig)
    }
    val config4 = {
      val twoDimPhConfig = TwoDimPheromoneConfig(
        10, GetType.ExponentialRandomMax.toString, UpdateType.PartFromIndex.toString
      )
      val pheromoneConfig = PheromoneConfig(
        PheromoneType.TwoDim.toString, phDimension, 0.05, 0.05, minValue, maxValue,
        SolutionsSelectionStrategy.LastIterationPareto.toString, 100, twoDimPhConfig
      )
      AlgorithmConfig(antsNum, iterationsNum, alpha, beta, pheromoneConfig)
    }
    val config6 = {
      val twoDimPhConfig = TwoDimPheromoneConfig(
        4, GetType.ExponentialRandomMax.toString, UpdateType.PartFromEvaluation.toString
      )
      val pheromoneConfig = PheromoneConfig(
        PheromoneType.TwoDim.toString, phDimension, 0.05, 0.05, minValue, maxValue,
        SolutionsSelectionStrategy.LastIterationPareto.toString, 10, twoDimPhConfig
      )
      AlgorithmConfig(antsNum, iterationsNum, alpha, beta, pheromoneConfig)
    }
    val config7 = {
      val twoDimPhConfig = TwoDimPheromoneConfig(
        4, GetType.ExponentialRandom.toString, UpdateType.PartFromEvaluation.toString
      )
      val pheromoneConfig = PheromoneConfig(
        PheromoneType.TwoDim.toString, phDimension, 0.05, 0.05, minValue, maxValue,
        SolutionsSelectionStrategy.LastIterationAll.toString, 30, twoDimPhConfig
      )
      AlgorithmConfig(antsNum, iterationsNum, alpha, beta, pheromoneConfig)
    }
    val config8 = {
      val twoDimPhConfig = TwoDimPheromoneConfig(
        20, GetType.ExponentialRandom.toString, UpdateType.PartFromEvaluation.toString
      )
      val pheromoneConfig = PheromoneConfig(
        PheromoneType.TwoDim.toString, phDimension, 0.05, 0.05, minValue, maxValue,
        SolutionsSelectionStrategy.LastIterationAll.toString, 60, twoDimPhConfig
      )
      AlgorithmConfig(antsNum, iterationsNum, alpha, beta, pheromoneConfig)
    }
    val config12 = {
      val twoDimPhConfig = TwoDimPheromoneConfig(
        20, GetType.ExponentialRandomMax.toString, UpdateType.PartFromEvaluation.toString
      )
      val pheromoneConfig = PheromoneConfig(
        PheromoneType.TwoDim.toString, phDimension, 0.1, 0.1, minValue, maxValue,
        SolutionsSelectionStrategy.LastIterationPareto.toString, 60, twoDimPhConfig
      )
      AlgorithmConfig(antsNum, iterationsNum, alpha, beta, pheromoneConfig)
    }
    val config44 = {
      val pheromoneConfig = PheromoneConfig(
        PheromoneType.Basic.toString, phDimension, 0.05, 0.05, minValue, maxValue,
        SolutionsSelectionStrategy.LastIterationPareto.toString, 10, new TwoDimPheromoneConfig() // irrelevant
      )
      AlgorithmConfig(antsNum, iterationsNum, alpha, beta, pheromoneConfig)
    }

    val config45 = {
      val pheromoneConfig = PheromoneConfig(
        PheromoneType.Basic.toString, phDimension, 0.05, 0.05, minValue, maxValue,
        SolutionsSelectionStrategy.LastIterationAll.toString, 10, new TwoDimPheromoneConfig() // irrelevant
      )
      AlgorithmConfig(antsNum, iterationsNum, alpha, beta, pheromoneConfig)
    }

    val algConfigs = List(
      config1,
      config2,
      config4,
      config6,
      config7,
      config8,
      config12,
      config44,
      config45
    )

    val ec: ExecutionContext = ExecutionContextHelper.fixed("worker", size = 12)
    val futures = for {
      problemInstances <- problemInstances
      algorithmConfig <- algConfigs
    } yield {
      val problemConfig = ProblemConfig(problemType, problemInstances.asJava, repeat, algorithmConfig)

      Future {
        println(problemConfig)
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

object CmdMain {
  import org.rogach.scallop._
  class Config(arguments: Seq[String]) extends ScallopConf(arguments) {
    val instance: ScallopOption[String] = opt[String]()
    val seed: ScallopOption[Long] = opt[Long]()
    val alpha: ScallopOption[Double] = opt[Double]()
    val beta: ScallopOption[Double] = opt[Double]()
    val antsNum: ScallopOption[Int] = opt[Int](default = Some(100))
    val iterations: ScallopOption[Int] = opt[Int](default = Some(200))
    val pheromoneType: ScallopOption[String] = choice(PheromoneType.values.map(_.toString))
    val solutionsSelectionStrategy: ScallopOption[String] = choice(SolutionsSelectionStrategy.values.map(_.toString)) // might require default = Some("LastIterationAll") for old TSP irace configurations
    val twoDimPhSize: ScallopOption[Int] = opt[Int]()
    val updateType: ScallopOption[String] = choice(TwoDimPheromoneConfig.UpdateType.values.map(_.toString))
    val getType: ScallopOption[String] = choice(TwoDimPheromoneConfig.GetType.values.map(_.toString))
    val pheromoneDelta: ScallopOption[Double] = opt[Double]()
    val updateAnts: ScallopOption[Int] = opt[Int]()

    verify()
  }

  def main(args: Array[String]): Unit = {
    val conf = new Config(args)

    val phDimension = 1 // stale
    val phDelta = conf.pheromoneDelta()
    val phIncrement = phDelta
    val phExtinction = phDelta
    val phMinValue = 0.001
    val phMaxValue = 0.999
    val takenAntsToPheromoneUpdate = conf.updateAnts()

    val pheromoneConfig = PheromoneType.valueOf(conf.pheromoneType()) match
      case PheromoneType.Basic =>
        assert(conf.twoDimPhSize.isEmpty)
        assert(conf.getType.isEmpty)
        assert(conf.updateType.isEmpty)
        PheromoneConfig(
          conf.pheromoneType(), phDimension, phIncrement, phExtinction, phMinValue, phMaxValue,
          conf.solutionsSelectionStrategy(), takenAntsToPheromoneUpdate, new TwoDimPheromoneConfig()
        )
      case PheromoneType.TwoDim =>
        val twoDimSize = conf.twoDimPhSize()
        val getType = conf.getType()
        val updateType = conf.updateType()
        val twoDimConfig = TwoDimPheromoneConfig(twoDimSize, getType, updateType)
        PheromoneConfig(
          conf.pheromoneType(), phDimension, phIncrement, phExtinction, phMinValue, phMaxValue,
          conf.solutionsSelectionStrategy(), takenAntsToPheromoneUpdate, twoDimConfig
        )

    val problemType = "tsp" // or "tsp" for now
    val instance = conf.instance()
    val folder = problemType match {
      case "tsp" => "mtsp"
      case "cvrp" if instance.startsWith("A")=> "cvrp/A"
      case "cvrp" if instance.startsWith("B")=> "cvrp/B"
      case "cvrp" => "cvrp/dim100plus"
      case _ => throw new IllegalArgumentException("Invalid problem type and/or instance")
    }
    val extension = problemType match {
      case "tsp" => "tsp"
      case "cvrp" => "vrp"
    }
    val problemInstance = s"$folder/$instance.$extension"
    val problemFiles = List(problemInstance).asJava
    val repeat = 1

    val antsNum = conf.antsNum()
    val iterations = conf.iterations()
    val alpha = conf.alpha()
    val beta = conf.beta()
    val algorithmConfig = AlgorithmConfig(antsNum, iterations, alpha, beta, pheromoneConfig)
    val problemConfig = ProblemConfig(problemType, problemFiles, repeat, algorithmConfig)

    val seed = conf.seed.toOption
    val logger = new IraceSingleObjectiveStdOutLogger
    Main.runConfiguration(problemConfig, seed, Some(logger))
  }
}