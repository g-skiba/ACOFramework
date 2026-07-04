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

import java.util.concurrent.TimeUnit
import scala.io.Source
import scala.jdk.CollectionConverters.*

object JMetalMKPRunner {
  private def runMKP(filename: String): Unit = {
    def populationToParetoPoints(population: java.util.List[BinarySolution]): Seq[IndexedSeq[Double]] = {
      val feasible = population.asScala.filter(ConstraintHandling.isFeasible(_))
      println(s"Population size: ${population.size()}, feasible: ${feasible.size}")
      if (feasible.isEmpty) {
        population.asScala.filterNot(ConstraintHandling.isFeasible).foreach { s =>
          println(s"Infeasible objectives: ${s.objectives().mkString(", ")}")
          println(s"Constraints: ${s.constraints().mkString(", ")}")
          println(s"Selected bits: ${s.variables().asScala.map(_.cardinality()).sum}")
        }
      }

      val asScala = feasible.map(_.objectives().toIndexedSeq).toIndexedSeq
      val pareto = asScala.iterator.zip(getParetoFrontMin(asScala)(identity).iterator).collect {
        case (v, true) => v
      }.toVector

      println(s"Feasible solutions: ${asScala.size}, pareto solutions: ${pareto.size}")
      pareto
    }

    val problemPath = "src/main/resources/mokpInstances/" + filename
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

    val maxEvaluations = 1000 //10000000
//    val maxEvaluations = 1000000
    val populationSize = 100

    def runTimed(
      algoName: String,
      runAlgorithm: => java.util.List[BinarySolution]
    ): (Seq[IndexedSeq[Double]], Long) = {
      println(s"Running $algoName...")
      val start = System.nanoTime()
      val result = runAlgorithm
      val end = System.nanoTime()
      (populationToParetoPoints(result), end - start)
    }

    def runNsgaii(): (Seq[IndexedSeq[Double]], Long) = {
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

    def runSpea2(): (Seq[IndexedSeq[Double]], Long) = {
      val spea2 = new SPEA2Builder(problem, crossover, mutation)
        .setSelectionOperator(selection)
        .setPopulationSize(populationSize)
        .setMaxIterations(maxEvaluations / populationSize)
        .build()

      runTimed("SPEA2", {
        spea2.run()
        spea2.result
      })
    }

    def runPesa2(): (Seq[IndexedSeq[Double]], Long) = {
      val pesa2 = new PESA2Builder(problem, crossover, mutation)
        .setPopulationSize(populationSize)
        .setArchiveSize(100)
        .setBisections(8) //5 originally, 8 in moea?
        .setMaxEvaluations(maxEvaluations)
        .build()

      runTimed("PESA2", {
        pesa2.run()
        pesa2.result
      })
    }

    val algorithmsToRun = Seq(
      "NSGA-II" -> (() => runNsgaii()),
      "SPEA2" -> (() => runSpea2()),
      "PESA2" -> (() => runPesa2())
    )

    def printResults(algoName: String, population: Seq[IndexedSeq[Double]], timeNano: Long): Unit = {
      println(s"\n--- $algoName Pareto Front sample ---")
      val everyNthSolution = 1 //10

      population.sortBy(_(0)).sliding(1, everyNthSolution).foreach { solution =>
        val s = solution.head
        // MKP is a maximization problem so its implementation negates objectives to keep it a minimization problem
        // Changing back to positive values for a human-readable output
        println(s"${-s(0)} ${-s(1)}")
      }
      if (population.isEmpty) println("No feasible Pareto points")
      else println(hvCalculator.calculateRemainingPartFromUnsorted(population))
      println(s"Run took: ${TimeUnit.NANOSECONDS.toSeconds(timeNano)}s")
    }

    algorithmsToRun.foreach { case (algoName, runAlgorithm) =>
      val (result, timeNano) = runAlgorithm()
      printResults(algoName, result, timeNano)
    }
  }

  def main(args: Array[String]): Unit = {
//    runMKP("1A/2KP50-11.dat")
    runMKP("1B-B/2KP50-1B.dat")
  }
}
