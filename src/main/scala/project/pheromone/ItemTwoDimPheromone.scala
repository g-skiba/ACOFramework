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

  val pheromone: Array[Array[Array[Double]]] =
    Array.fill(pheromoneDimension)(Array.fill(maxItemNumber + 1)(Array.fill(twoDimPheromoneSize)(maxValue)))
  private var currentMin = maxValue

  override def getPheromone(edge: Edge): Array[Double] = {
    val item = edge.node2.number

    def calculate = {
      Array.tabulate[Double](pheromoneDimension) { dim =>
        val phValues = pheromone(dim)(item)

        getType match {
          case GetType.ExponentialRandom => exponentialRandom(phValues, maxUpTo = false)
          case GetType.ExponentialRandomMax => exponentialRandom(phValues, maxUpTo = true)
          case GetType.WeightedCombination => weightedCombination(phValues)
          case GetType.PairingCombination => pairingCombination(phValues)
          case GetType.ExpectedCombination => expectedCombination(phValues)
        }
      }
    }

//    if (getType.isRandomized) calculate
//    else cache.getOrElseUpdate(edge.cantorValue, calculate)
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
    val weightedValues = (0 until twoDimPheromoneSize).iterator.map { i =>
      val weight =
        if (i == twoDimPheromoneSize - 1) 1 / math.pow(2, i - 1)
        else 1 / math.pow(2, i)
      if (weightedSum) values(i) * weight else math.pow(values(i), weight)
    }
    val value = if (weightedSum) weightedValues.sum else weightedValues.product
    if (detailedDebug) println((value, values.mkString("[", ",", "]")))
    value
  }

  private def pairingCombination(values: Array[Double]): Double = {
    val value = (0 until twoDimPheromoneSize / 2).iterator.map { i =>
      val pos = values(i)
      val neg = values(twoDimPheromoneSize - i - 1)
      val v = ((pos + neg) / 2) + (pos - neg) * (twoDimPheromoneSize / 2 - i)
      if (detailedDebug) debug(v)
      v
    }.sum / (twoDimPheromoneSize / 2)

    if (detailedDebug) debug((value, values.mkString("[", ",", "]")))
    ensureMinMax(value, minV = currentMin)
  }

  private def expectedCombination(values: Array[Double]): Double = {
    val sum = values.sum
    val expectedValue = values.reverseIterator.zipWithIndex.map { case (v, ind) =>
      val partScore = (ind + 0.5) / twoDimPheromoneSize
      v / sum * partScore
    }.sum
    val min = values.min
    val max = values.max
    val v = min + (max - min) * expectedValue
    if (detailedDebug) debug((expectedValue, v, values.mkString("[", ",", "]")))
    v
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
    double.min(maxV).max(minV)
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
