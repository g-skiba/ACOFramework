package project.pheromone

import project.config.TwoDimPheromoneConfig.{GetType, UpdateType}
import project.config.{SolutionsSelectionStrategy, TwoDimPheromoneConfig}
import project.graph.Edge
import project.logging.DebugLogger.debug
import project.repo.BaseSolutionRepo

import scala.annotation.tailrec
import scala.util.Random

class ItemTwoDimPheromone(
  edges: Seq[Edge],
  val increment: Double,
  val extinction: Double,
  val pheromoneDimension: Int,
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
  debug(s"Creating item two dim pheromone table with $pheromoneDimension dimensions, $twoDimPheromoneSize pheromone size and $updateAnts update ants")
  PheromoneUpdateSelection.validateBucketConfiguration(solutionsSelectionStrategy, updateType, twoDimPheromoneSize, getType)

  private val maxItemNumber = edges.iterator.map(_.node2.number).max
  private val cache: ArrayCache[Array[Double]] = new ArrayCache[Array[Double]](maxItemNumber)
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

  val pheromone: Array[Array[Array[Double]]] =
    Array.fill(pheromoneDimension)(Array.fill(maxItemNumber + 1)(Array.fill(twoDimPheromoneSize)(maxValue)))
  private var currentMin = maxValue

  override def getPheromone(edge: Edge): Array[Double] = {
    val item = edge.node2.number

    def calculate: Array[Double] = {
      val result = new Array[Double](pheromoneDimension)

      var dim = 0
      while (dim < result.length) {
        val phValues = pheromone(dim)(item)

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
//    else cache.getOrElseUpdate(item, calculate)
    cache.getOrElseUpdate(item, calculate)
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

      maxUpToInd(0.0, 0)
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

      total += v
      i += 1
    }

    val value = total / half

    if (detailedDebug) debug((value, values.mkString("[", ",", "]")))
    ensureMinMax(value, minV = currentMin)
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

      debug(s"Updating item pheromone for dimension $dim with ${solutionsWithParts.size} solutions - ${solutionsWithParts.map(_._1.evaluation)}")
      val solutionsByPart = solutionsWithParts.groupBy(_._2)
      if (detailedDebug) debug(solutionsByPart.toList.map { case (part, entries) => (part, entries.size) }.sortBy(_._1))

      solutionsByPart
        .foreach { case (part, partSolutions) =>
          val partIncrement = increment / partSolutions.size
          partSolutions.foreach { case (solution, _) =>
            solution.solution.iterator.drop(1).foreach { node =>
              val pheromones = pheromone(dim)(node.number)
              pheromones(part) += increment //???
//              pheromones(part) += partIncrement // should be this instead?
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
