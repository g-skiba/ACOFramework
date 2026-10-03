package refalg

import org.uma.jmetal.algorithm.multiobjective.nsgaii.NSGAIIBuilder
import org.uma.jmetal.algorithm.multiobjective.pesa2.PESA2Builder
import org.uma.jmetal.algorithm.multiobjective.spea2.SPEA2Builder
import org.uma.jmetal.operator.crossover.impl.{PMXCrossover, SinglePointCrossover}
import org.uma.jmetal.operator.mutation.impl.{BitFlipMutation, PermutationSwapMutation}
import org.uma.jmetal.operator.selection.impl.BinaryTournamentSelection
import org.uma.jmetal.problem.multiobjective.multiobjectiveknapsack.MultiObjectiveKnapsack
import org.uma.jmetal.problem.multiobjective.multiobjectivetsp.MultiObjectiveTSP
import org.uma.jmetal.solution.binarysolution.BinarySolution
import org.uma.jmetal.solution.permutationsolution.PermutationSolution
import org.uma.jmetal.util.ConstraintHandling
import org.uma.jmetal.util.comparator.constraintcomparator.impl.OverallConstraintViolationDegreeComparator
import org.uma.jmetal.util.comparator.dominanceComparator.impl.DominanceWithConstraintsComparator
import mokp.MokpParser
import pareto.getParetoFrontMin
import project.problem.MokpProblem
import tsp.TspReader

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}
import java.util.concurrent.TimeUnit
import scala.io.Source
import scala.jdk.CollectionConverters.*

object JMetalMKPRunner {
  private case class PopulationSummary(points: Seq[IndexedSeq[Double]], feasibleSolutions: Int)
  private case class AlgorithmRunSummary(
    instance: String,
    repeat: Int,
    algorithm: String,
    hvIndicator: Double,
    feasibleSolutions: Int,
    paretoSolutions: Int,
    timeNano: Long
  ) {
    def toCsv: String = Seq(
      csvField(instance),
      repeat.toString,
      csvField(algorithm),
      hvIndicator.toString,
      (timeNano.toDouble / 1e9).toString,
      feasibleSolutions.toString,
      paretoSolutions.toString,
      if (feasibleSolutions > 0) "OK" else "NO_FEASIBLE"
    ).mkString(",")
  }

  private case class RunnerOptions(filenames: Seq[String], repeats: Int, output: Option[String])

  private val csvHeader =
    "instance,repeat,algorithm,hv_indicator,time_seconds,feasible_solutions,pareto_solutions,status"
  private val algorithmNames = Seq(/*"NSGA-II", */"SPEA2", "PESA2")

  private def csvField(value: String): String = "\"" + value.replace("\"", "\"\"") + "\""

  private def printHvTables(summaries: Seq[AlgorithmRunSummary]): Unit = {
    summaries.map(_.instance).distinct.foreach { instance =>
      val instanceSummaries = summaries.filter(_.instance == instance)
      println(s"\n=== $instance ===")
      println(algorithmNames.mkString(","))
      instanceSummaries.map(_.repeat).distinct.sorted.foreach { repeat =>
        val repeatSummaries = instanceSummaries.filter(_.repeat == repeat).map(summary => summary.algorithm -> summary).toMap
        val hvValues = algorithmNames.map(algorithm => repeatSummaries(algorithm).hvIndicator.toString)
        println(hvValues.mkString(","))
      }
    }
  }

  private def parseArgs(args: Array[String]): RunnerOptions = {
    val filenames = Seq.newBuilder[String]
    var repeats = 1
    var output: Option[String] = None
    var index = 0
    while (index < args.length) {
      args(index) match {
        case "--repeats" =>
          require(index + 1 < args.length, "--repeats requires a positive integer")
          repeats = args(index + 1).toInt
          require(repeats > 0, "--repeats must be positive")
          index += 2
        case "--output" =>
          require(index + 1 < args.length, "--output requires a CSV file path")
          require(output.isEmpty, "--output may be supplied only once")
          output = Some(args(index + 1))
          index += 2
        case option if option.startsWith("--") =>
          throw new IllegalArgumentException(s"Unknown option: $option")
        case filename =>
          filenames += filename
          index += 1
      }
    }
    val selectedFilenames = filenames.result()
    RunnerOptions(
      if (selectedFilenames.nonEmpty) selectedFilenames else Seq("1B-A/2KP450-1A"),
      repeats,
      output
    )
  }

  private def normalizeFilename(filename: String): String =
    if (filename.endsWith(".dat")) filename else s"$filename.dat"

  private def runMKP(
    filename: String,
    repeat: Int,
    printDetails: Boolean,
    log: String => Unit,
    onSummary: AlgorithmRunSummary => Unit
  ): Seq[AlgorithmRunSummary] = {
    val normalizedFilename = normalizeFilename(filename)
    log(s"\n=== $normalizedFilename, repeat $repeat ===")

    def populationToParetoPoints(population: java.util.List[BinarySolution]): PopulationSummary = {
      val feasible = population.asScala.filter(ConstraintHandling.isFeasible(_))
      log(s"Population size: ${population.size()}, feasible: ${feasible.size}")
      if (feasible.isEmpty && printDetails) {
        population.asScala.filterNot(ConstraintHandling.isFeasible).foreach { s =>
          log(s"Infeasible objectives: ${s.objectives().mkString(", ")}")
          log(s"Constraints: ${s.constraints().mkString(", ")}")
          log(s"Selected bits: ${s.variables().asScala.map(_.cardinality()).sum}")
        }
      }

      val asScala = feasible.map(_.objectives().toIndexedSeq).toIndexedSeq
      val pareto = asScala.iterator.zip(getParetoFrontMin(asScala)(identity).iterator).collect {
        case (v, true) => v
      }.toVector

      log(s"Feasible solutions: ${asScala.size}, pareto solutions: ${pareto.size}")
      PopulationSummary(pareto, asScala.size)
    }

    val problemPath = "src/main/resources/mokpInstances/" + normalizedFilename
    val hvCalculator = {
      val data = MokpParser.read(Source.fromFile(problemPath))
      new MokpProblem(data).getHypervolumeCalculator.get
    }

    val problem = MultiObjectiveKnapsack.readMKP(problemPath)

    // Configure selection, crossover, and mutation operators
    val crossover = new SinglePointCrossover[BinarySolution](0.9)
    val mutationProbability = 1.0 / problem.totalNumberOfBits
    val mutation = new BitFlipMutation[BinarySolution](mutationProbability)
    val dominanceComparator = new DominanceWithConstraintsComparator(new OverallConstraintViolationDegreeComparator[BinarySolution])
//    val selection = new BinaryTournamentSelection[BinarySolution]()
    val selection = new BinaryTournamentSelection[BinarySolution](dominanceComparator)

//    val maxEvaluations = 1000000
    val maxEvaluations = problem.totalNumberOfBits match {
      case 150 =>  2500000
      case 250 =>  5000000
      case 350 => 10000000
      case 450 => 12500000
    }
    val populationSize = 100

    def runTimed(
      algoName: String,
      runAlgorithm: => java.util.List[BinarySolution]
    ): (PopulationSummary, Long) = {
      log(s"Running $algoName...")
      val start = System.nanoTime()
      val result = runAlgorithm
      val end = System.nanoTime()
      (populationToParetoPoints(result), end - start)
    }

    def runNsgaii(): (PopulationSummary, Long) = {
      val nsgaii = new NSGAIIBuilder(problem, crossover, mutation, populationSize)
        .setSelectionOperator(selection)
        .setDominanceComparator(dominanceComparator)
        .setMaxEvaluations(maxEvaluations)
        .build()

      runTimed("NSGA-II", {
        nsgaii.run()
        nsgaii.result
      })
    }

    def runSpea2(): (PopulationSummary, Long) = {
      val spea2 = new SPEA2Builder(problem, crossover, mutation)
        .setSelectionOperator(selection)
        .setPopulationSize(populationSize)
        .setMaxIterations(maxEvaluations / populationSize / 4)
        .build()

      runTimed("SPEA2", {
        spea2.run()
        spea2.result
      })
    }

    def runPesa2(): (PopulationSummary, Long) = {
      val pesa2 = new PESA2Builder(problem, crossover, mutation)
        .setPopulationSize(populationSize)
        .setArchiveSize(100)
        .setBisections(8) //5 originally, 8 in moea?
        .setMaxEvaluations(maxEvaluations * 5)
        .build()

      runTimed("PESA2", {
        pesa2.run()
        pesa2.result
      })
    }

    val algorithmRunners = Map(
      "NSGA-II" -> (() => runNsgaii()),
      "SPEA2" -> (() => runSpea2()),
      "PESA2" -> (() => runPesa2())
    )
    val algorithmsToRun = algorithmNames.map(name => name -> algorithmRunners(name))

    def printResults(algoName: String, population: PopulationSummary, hvRemaining: Double, timeNano: Long): Unit = {
      if (printDetails) {
        log(s"\n--- $algoName Pareto Front sample ---")
        val everyNthSolution = 1 //10

        population.points.sortBy(_(0)).sliding(1, everyNthSolution).foreach { solution =>
          val s = solution.head
          // MKP is a maximization problem so its implementation negates objectives to keep it a minimization problem
          // Changing back to positive values for a human-readable output
          log(s"${-s(0)} ${-s(1)}")
        }
      }
      if (population.points.isEmpty) log("No feasible Pareto points")
      log(hvRemaining.toString)
      log(s"Run took: ${TimeUnit.NANOSECONDS.toSeconds(timeNano)}s")
    }

    algorithmsToRun.map { case (algoName, runAlgorithm) =>
      val (result, timeNano) = runAlgorithm()
      // An empty feasible front has zero dominated area, so its remaining-area score is 1.0.
      val hvRemaining = hvCalculator.calculateRemainingPartFromUnsorted(result.points)
      printResults(algoName, result, hvRemaining, timeNano)
      if (!printDetails) {
        log(
          s"$algoName summary: feasible=${result.feasibleSolutions}, pareto=${result.points.size}, time=${TimeUnit.NANOSECONDS.toSeconds(timeNano)}s"
        )
      }
      val summary = AlgorithmRunSummary(
        normalizedFilename, repeat, algoName, hvRemaining, result.feasibleSolutions, result.points.size, timeNano
      )
      onSummary(summary)
      summary
    }
  }

  def main(args: Array[String]): Unit = {
    val options = parseArgs(args)
    val outputWriter = options.output.map { path =>
      Files.newBufferedWriter(Paths.get(path), StandardCharsets.UTF_8, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)
    }
    try {
      outputWriter.foreach { writer =>
        writer.write(csvHeader)
        writer.newLine()
        writer.flush()
      }
      val summaries = for {
        filename <- options.filenames
        repeat <- 1 to options.repeats
        summary <- runMKP(
          filename,
          repeat,
          printDetails = options.filenames.size == 1 && options.repeats == 1,
          log = println,
          onSummary = summary => outputWriter match {
            case Some(writer) =>
              writer.write(summary.toCsv)
              writer.newLine()
              writer.flush()
            case None => ()
          }
        )
      } yield summary

      if (outputWriter.isEmpty) {
        println("\n=== Results CSV ===")
        println(csvHeader)
        summaries.foreach(summary => println(summary.toCsv))
        printHvTables(summaries)
      }
      options.output.foreach(path => println(s"Results written to $path"))
    } finally {
      outputWriter.foreach(_.close())
    }
  }
}
