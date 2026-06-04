package refalg

import org.uma.jmetal.algorithm.multiobjective.nsgaii.NSGAIIBuilder
import org.uma.jmetal.algorithm.multiobjective.spea2.SPEA2Builder
import org.uma.jmetal.algorithm.multiobjective.pesa2.PESA2Builder
import org.uma.jmetal.operator.crossover.impl.PMXCrossover
import org.uma.jmetal.operator.mutation.impl.PermutationSwapMutation
import org.uma.jmetal.operator.selection.impl.BinaryTournamentSelection
import pareto.getParetoFrontMin
import tsp.{TspReader, TspsToMtsp}

import java.util.concurrent.TimeUnit
import scala.io.Source

import org.uma.jmetal.problem.multiobjective.multiobjectivetsp.MultiObjectiveTSP
import org.uma.jmetal.solution.permutationsolution.PermutationSolution
import scala.jdk.CollectionConverters._

object JMetalRunner {
  private def runMtsp(distanceFile: String, costFile: String): Unit = {
    def filePathFromResources(filename: String) = {
      s"mtsp/$filename.tsp"
    }

    def filePathFromRoot(filename: String) = {
      s"src/main/resources/${filePathFromResources(filename)}"
    }

    def populationToParetoPoints(population: java.util.List[PermutationSolution[Integer]]): Seq[IndexedSeq[Double]] = {
      val asScala = population.asScala.map(_.objectives().toIndexedSeq).toIndexedSeq
      asScala.iterator.zip(getParetoFrontMin(asScala)(identity).iterator).collect {
        case (v, true) => v
      }.toVector
    }

    val hvCalculator = {
      def tspFromFile(filename: String) = TspReader.read(Source.fromResource(filePathFromResources(filename)))

      val (_, mtspProblem) = TspsToMtsp(Iterable(tspFromFile(distanceFile), tspFromFile(costFile)))
      mtspProblem.getHypervolumeCalculator.get
    }

    // Note: You must have kroA100.tsp and kroB100.tsp in the execution directory
    // Download them from standard TSP Libs or jMetal's resource folder on GitHub
    val problem = new MultiObjectiveTSP(List(filePathFromRoot(distanceFile), filePathFromRoot(costFile)).asJava)

    // Configure selection, crossover, and mutation operators
    val crossover = new PMXCrossover(0.9)
    val mutation = new PermutationSwapMutation[Integer](0.2)
    val selection = new BinaryTournamentSelection[PermutationSolution[Integer]]()

    val maxEvaluations = 10000000
    val populationSize = 100

    // 1. Setup and Run NSGA-II
    val nsgaii = new NSGAIIBuilder[PermutationSolution[Integer]](problem, crossover, mutation, populationSize)
      .setSelectionOperator(selection)
      .setMaxEvaluations(maxEvaluations)
      .build()

    println("Running NSGA-II...")
    val nsgaii2Start = System.nanoTime()
    nsgaii.run()
    val nsgaii2End = System.nanoTime()
    val nsgaiiResult = populationToParetoPoints(nsgaii.result)
    val nsgaiiTime = nsgaii2End - nsgaii2Start

    // 2. Setup and Run SPEA2
    val spea2 = new SPEA2Builder[PermutationSolution[Integer]](problem, crossover, mutation)
      .setSelectionOperator(selection)
      .setPopulationSize(populationSize)
      .setMaxIterations(maxEvaluations / populationSize)
      .build()

    println("Running SPEA2...")
    val spea2Start = System.nanoTime()
    spea2.run()
    val spea2End = System.nanoTime()
    val spea2Result = populationToParetoPoints(spea2.result)
    val spea2Time = spea2End - spea2Start

    // 3. Setup and Run PESA2
    val pesa2 = new PESA2Builder[PermutationSolution[Integer]](problem, crossover, mutation)
      .setPopulationSize(populationSize)
      .setArchiveSize(100)
      .setBisections(8) //5 originally, 8 in moea?
      .setMaxEvaluations(maxEvaluations)
      .build()

    println("Running PESA2...")
    val pesa2Start = System.nanoTime()
    pesa2.run()
    val pesa2End = System.nanoTime()
    val pesa2Result = populationToParetoPoints(pesa2.result)
    val pesa2Time = pesa2End - pesa2Start

    def printResults(algoName: String, population: Seq[IndexedSeq[Double]], timeNano: Long): Unit = {
      println(s"\n--- $algoName Pareto Front sample ---")
      val everyNthSolution = 1 //10
      
      population.sortBy(_(0)).sliding(1, everyNthSolution).foreach { solution =>
        val s = solution.head
        println(s"${s(0)} ${s(1)}")
      }
      println(hvCalculator.calculateRemainingPartFromUnsorted(population))
      println(s"Run took: ${TimeUnit.NANOSECONDS.toSeconds(timeNano)}s")
    }

    printResults("NSGA-II", nsgaiiResult, nsgaiiTime)
    printResults("SPEA2", spea2Result, spea2Time)
    printResults("PESA2", pesa2Result, pesa2Time)
  }

  def main(args: Array[String]): Unit = {
//    runMtsp("kroA100", "kroB100")
    runMtsp("euclidA500", "euclidB500")
  }
}