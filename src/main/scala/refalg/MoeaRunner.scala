package refalg

import org.moeaframework.Executor
import org.moeaframework.core.{NondominatedPopulation, Solution}
import org.moeaframework.problem.AbstractProblem

import scala.util.Random
import scala.jdk.CollectionConverters.*
import org.moeaframework.core.Solution
import org.moeaframework.core.variable.Permutation
import org.moeaframework.problem.AbstractProblem
import org.moeaframework.algorithm.NSGAII
import org.moeaframework.algorithm.SPEA2
import org.moeaframework.algorithm.PESA2
import tsp.{TspReader, TspsToMtsp}

import java.util.concurrent.TimeUnit
import scala.io.Source
import scala.math.{round, sqrt}
import scala.jdk.CollectionConverters.*

// 1. Helper Object to parse TSPLIB files
object TSPReader {
  def read(filename: String): Array[Array[Double]] = {
    // Read all lines from the file
    val lines = Source.fromResource(filename).getLines().toList

    // Extract only the coordinate lines
    val coordLines = lines
      .dropWhile(line => !line.trim.startsWith("NODE_COORD_SECTION"))
      .drop(1)
      .takeWhile(line => line.trim != "EOF" && line.trim.nonEmpty)

    // Parse X and Y coordinates (ignoring the city ID at index 0)
    val coords = coordLines.map { line =>
      val parts = line.trim.split("\\s+")
      (parts(1).toDouble, parts(2).toDouble)
    }.toArray

    val n = coords.length
    val distMatrix = Array.ofDim[Double](n, n)

    // Calculate 2D Euclidean distances (TSPLIB EUC_2D rounds to the nearest integer)
    for (i <- 0 until n; j <- 0 until n) {
      val dx = coords(i)._1 - coords(j)._1
      val dy = coords(i)._2 - coords(j)._2
      distMatrix(i)(j) = round(sqrt(dx * dx + dy * dy)).toDouble
    }

    distMatrix
  }
}

// 2. Updated Bi-objective TSP Problem using the parser
class BiobjectiveTSP(fileA: String, fileB: String) extends AbstractProblem(1, 2) {
  // Load distance matrices dynamically from the provided files
  private val distA = TSPReader.read(fileA)
  private val distB = TSPReader.read(fileB)

  require(distA.length == distB.length, "Both TSP files must have the same number of cities!")

  val numberOfCities: Int = distA.length

  // Initialization: Provide 1 Permutation variable per solution
  override def newSolution(): Solution = {
    val solution = new Solution(1, 2)
    solution.setVariable(0, new Permutation(numberOfCities))
    solution
  }

  // Evaluation phase
  override def evaluate(solution: Solution): Unit = {
    val perm = solution.getVariable(0).asInstanceOf[Permutation].toArray
    var costA = 0.0
    var costB = 0.0

    for (i <- 0 until numberOfCities - 1) {
      costA += distA(perm(i))(perm(i + 1))
      costB += distB(perm(i))(perm(i + 1))
    }
    // Complete the loop back to the origin
    costA += distA(perm(numberOfCities - 1))(perm(0))
    costB += distB(perm(numberOfCities - 1))(perm(0))

    solution.setObjective(0, costA)
    solution.setObjective(1, costB)
  }
}

object MoeaRunner {
  def main(args: Array[String]): Unit = {
    // Make sure kroA100.tsp and kroB100.tsp are in your project root!
//    val distanceFile = "kroA100"
//    val costFile = "kroB100"

    val distanceFile = "euclidA500"
    val costFile = "euclidB500"

    def filePathFromResources(filename: String) = {
      s"mtsp/$filename.tsp"
    }

    println(s"Loading problem instances: $distanceFile and $costFile...")
    val problem = new BiobjectiveTSP(filePathFromResources(distanceFile), filePathFromResources(costFile))

    val hvCalculator = {
      def tspFromFile(filename: String) = TspReader.read(Source.fromResource(filePathFromResources(filename)))

      val (_, mtspProblem) = TspsToMtsp(Iterable(tspFromFile(distanceFile), tspFromFile(costFile)))
      mtspProblem.getHypervolumeCalculator.get
    }



    def populationToParetoPoints(population: NondominatedPopulation): Seq[IndexedSeq[Double]] = {
      population.asScala.toList.map(p => IndexedSeq(p.getObjective(0), p.getObjective(1)))
    }

    val maxEvaluations = 10000000

    val algorithms = Map(
      "NSGA-II" -> new NSGAII(problem),
      "SPEA2" -> new SPEA2(problem),
      "PESA2" -> new PESA2(problem)
    )

    for ((name, algorithm) <- algorithms) {
      println(s"\nRunning $name via MOEA Framework...")

      val start = System.nanoTime()
      algorithm.run(maxEvaluations)
      val end = System.nanoTime()
      val solutions = populationToParetoPoints(algorithm.getResult)

      println(s"--- $name Pareto Front sample ---")
      val everyNthSolution = 1 //10

      solutions.sortBy(_(0)).sliding(1, everyNthSolution)/*.take(5)*/.foreach { solution =>
        val s = solution.head
        println(s"${s(0)} ${s(1)}")
      }
      println(hvCalculator.calculateRemainingPartFromUnsorted(solutions))
      println(s"Run took: ${TimeUnit.NANOSECONDS.toSeconds(end - start)}s")
      
//      if (solutions.size > 5) println("... (truncated)")
    }
  }
}
