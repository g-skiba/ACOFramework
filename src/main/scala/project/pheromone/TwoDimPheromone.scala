package project.pheromone

import project.config.{SolutionsSelectionStrategy, TwoDimPheromoneConfig}
import project.config.TwoDimPheromoneConfig.{GetType, UpdateType}
import project.graph.Edge
import project.logging.DebugLogger.debug
import project.logging.WarnLogger.warn
import project.repo.BaseSolutionRepo

import scala.annotation.tailrec
import scala.collection.mutable.Map as MMap
import scala.util.Random

/** Assuming one-dimensional problem consider: keep best (or generally more)
  * solutions for pheromone update? range of solution cost (currently from
  * iteration solutions)
  */
class TwoDimPheromone(
  edges: Seq[Edge],
  val increment: Double,
  val extinction: Double,
  val pheromoneDimension: Int, // TODO for now assuming this is the same as the number of optimization targets
  minValue: Double,
  maxValue: Double,
  solutionsSelectionStrategy: SolutionsSelectionStrategy,
  updateAnts: Option[Int],
  twoDimPheromoneSize: Int,
  getType: TwoDimPheromoneConfig.GetType,
  updateType: TwoDimPheromoneConfig.UpdateType,
  rnd: Random,
  detailedDebug: Boolean = false
) extends BasePheromoneTable {
  debug(s"Creating two dim pheromone table with $pheromoneDimension dimensions, $twoDimPheromoneSize pheromone size and $updateAnts update ants")
  PheromoneUpdateSelection.validateBucketConfiguration(solutionsSelectionStrategy, updateType, twoDimPheromoneSize, getType)
  private val maxCantorValue = edges.iterator.map(e => e.cantorValue).max
  // indexed by edges' cantor value
  private val cache: ArrayCache[Array[Double]] = new ArrayCache[Array[Double]](maxCantorValue)
  private val exponentialWeights: Array[Double] = {
    val weights = new Array[Double](twoDimPheromoneSize)

    var i = 0
    while (i < weights.length) {
      val exponent =
        if (i == weights.length - 1) i - 1
        else i

      weights(i) = 1.0 / math.pow(2.0, exponent)
      i += 1
    }

    weights
  }

  // indexed by dim and edges' cantor value
  val pheromone: Array[Array[Array[Double]]] =
    Array.fill(pheromoneDimension)(Array.fill(maxCantorValue + 1)(Array.fill(twoDimPheromoneSize)(maxValue)))
  private var currentMin = maxValue

  override def getPheromone(edge: Edge): Array[Double] = {
    val edgeId = edge.cantorValue

    def calculate: Array[Double] = {
      val result = new Array[Double](pheromoneDimension)

      var dim = 0
      while (dim < result.length) {
        val phValues = pheromone(dim)(edgeId)

        result(dim) = getType match {
          case GetType.ExponentialRandom => exponentialRandom(phValues, maxUpTo = false)
          case GetType.ExponentialRandomMax => exponentialRandom(phValues, maxUpTo = true)
          case GetType.WeightedCombination => weightedCombination(phValues)
          case GetType.PairingCombination => pairingCombination(phValues)
          case GetType.ExpectedCombination => expectedCombination(phValues)
        }

        dim += 1
      }

      result
    }

//    if (getType.isRandomized) calculate
//    else cache.getOrElseUpdate(edge.cantorValue, calculate)
    cache.getOrElseUpdate(edgeId, calculate)
  }

  private def exponentialRandom(values: Array[Double], maxUpTo: Boolean): Double = {
    val random = rnd.nextInt((1 << twoDimPheromoneSize) - 1) + 1
    val log = math.log(random) / math.log(2)
    val index = twoDimPheromoneSize - 1 - log.toInt

    val res = if (maxUpTo) {
      @tailrec
      def maxUpToInd(curMax: Double, curInd: Int): Double = {
        if (curInd > index) curMax
        else maxUpToInd(values(curInd).max(curMax), curInd + 1)
      }

      val max = maxUpToInd(0.0, 0)
      max
    } else {
      values(index)
    }
    if (detailedDebug) debug(res)
    res
  }

  private def weightedCombination(values: Array[Double]): Double = {
    val weightedSum = false
    var value = if (weightedSum) 0.0 else 1.0
    var i = 0

    while (i < twoDimPheromoneSize) {
      if (weightedSum) value += values(i) * exponentialWeights(i)
      else value *= math.pow(values(i), exponentialWeights(i))
      i += 1
    }

    if (detailedDebug) println((value, values.mkString("[", ",", "]")))

    value
  }

  private def pairingCombination(values: Array[Double]): Double = {
    //these values could be precalculated / cached (per iteration)
    //pairing from outside to the center; within pairs calculate "final value" based on avg and diff
    // then the contrast between "positive" and "negative" values should be reinforced
    // finally calculate average of values got from pairs
    val half = twoDimPheromoneSize / 2
    var total = 0.0
    var i = 0

    while (i < half) {
      val pos = values(i)
      val neg = values(twoDimPheromoneSize - i - 1)
      val v =
        ((pos + neg) / 2.0) +
          (pos - neg) * (half - i)

      if (detailedDebug) debug(v)

      //alternatives: total += ...
//      ensureMinMax(v) //seems worse?
//      if (pos >= neg) 1.0 else 0.0 //too simple? ;)
      total += v
      i += 1
    }

    val value = total / half

    if (detailedDebug) debug((value, values.mkString("[", ",", "]")))

    //additional adjustments?
    ensureMinMax(value, minV = currentMin)
//    value
  }

  private def expectedCombination(values: Array[Double]): Double = {
    var sum = 0.0
    var min = values(0)
    var max = values(0)
    var i = 0

    while (i < values.length) {
      val value = values(i)
      sum += value
      min = java.lang.Math.min(min, value)
      max = java.lang.Math.max(max, value)
      i += 1
    }

    var expectedValue = 0.0
    var reverseIndex = 0
    i = values.length - 1

    while (i >= 0) {
      val partScore = (reverseIndex + 0.5) / twoDimPheromoneSize
      expectedValue += (values(i) / sum) * partScore

      i -= 1
      reverseIndex += 1
    }

    val result = min + (max - min) * expectedValue

    if (detailedDebug) debug((expectedValue, result, values.mkString("[", ",", "]")))

    result
  }

  override def pheromoneUpdate(solutionsRepo: BaseSolutionRepo): Unit = {
    val solutionGroups = solutionsSelectionStrategy(solutionsRepo)
    PheromoneUpdateSelection.validateFrontCount(solutionGroups.size, updateType, twoDimPheromoneSize)
    cache.clear()

    def updateDim(dim: Int): Unit = {
      val selectedGroups = PheromoneUpdateSelection.selectForObjective(solutionGroups, dim, updateAnts)
      val solutionsWithParts = PheromoneUpdateSelection.assignBuckets(selectedGroups, dim, updateType, twoDimPheromoneSize)

      debug(s"Updating pheromone for dimension $dim with ${solutionsWithParts.size} solutions - ${solutionsWithParts.map(_._1.evaluation)}")
      val solutionsByPart = solutionsWithParts.groupBy(_._2)
      if (detailedDebug) debug(solutionsByPart.toList.map { case (part, entries) => (part, entries.size) }.sortBy(_._1))

      solutionsByPart
        .foreach { case (part, partSolutions) =>
          val partIncrement = increment / partSolutions.size
          partSolutions.foreach { case (solution, _) =>
            solution.solution
              .sliding(2)
              .map(x => Edge(x.head, x.last))
              .foreach { edge =>
                val pheromones = pheromone(dim)(edge.cantorValue)
                pheromones(part) += increment
              }
          }
        }
    }

    for (dim <- 0 until pheromoneDimension) {
      updateDim(dim)
    }
  }

  private def ensureMinMax(double: Double, maxV: Double = maxValue, minV: Double = minValue): Double = {
    java.lang.Math.max(java.lang.Math.min(double, maxV), minV)
  }

  override def afterUpdatesAction(): Unit = {
    def extinctAndEnsureMinMax(double: Double): Double = {
      val e = double * (1 - extinction)
      ensureMinMax(e)
    }

    pheromone.foreach(_.foreach(_.mapInPlace(extinctAndEnsureMinMax)))
    currentMin = extinctAndEnsureMinMax(currentMin)
    if (detailedDebug) {
      pheromone.foreach { p =>
        val values = p.flatten
        debug((values.min, values.max, values.sum / values.length))
      }
    }
  }

}
