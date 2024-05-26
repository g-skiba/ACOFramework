package project.pheromone

import project.config.{SolutionsSelectionStrategy, TwoDimPheromoneConfig}
import project.config.TwoDimPheromoneConfig.{GetType, UpdateType}
import project.graph.Edge
import project.logging.DebugLogger.debug
import project.repo.BaseSolutionRepo
import project.solution.BaseSolution

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
  val pheromoneDimension: Int, // TODO for now ignored, we assume 1-dim problem
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
  debug(s"Creating two dim pheromone table with $pheromoneDimension dimensions (ignored), $twoDimPheromoneSize pheromone size and $updateAnts update ants")
  require(
    twoDimPheromoneSize % 2 == 0,
    "Temporary assumption for `getPheromone` based on pairing values starting from edges"
  )
  private val cache: MMap[Edge, Array[Double]] = MMap.empty

  val pheromone: MMap[Edge, IndexedSeq[Double]] =
    edges.map((_, IndexedSeq.fill(twoDimPheromoneSize)(maxValue))).to(MMap)
  private var currentMin = maxValue

  override def getPheromone(edge: Edge): Array[Double] = {
    getType match {
      case GetType.ExponentialRandom => exponentialRandom(edge, maxUpTo = false) //can't cache since it's randomized
      case GetType.ExponentialRandomMax => exponentialRandom(edge, maxUpTo = true) //can't cache since it's randomized
      case GetType.WeightedCombination => cache.getOrElseUpdate(edge, weightedCombination(edge))
      case GetType.PairingCombination => cache.getOrElseUpdate(edge, pairingCombination(edge))
      case GetType.ExpectedCombination => cache.getOrElseUpdate(edge, expectedCombination(edge))
    }
  }

  private def exponentialRandom(edge: Edge, maxUpTo: Boolean): Array[Double] = {
    val random = rnd.nextInt((1 << twoDimPheromoneSize) - 1) + 1
    val log = math.log(random) / math.log(2)
    val index = twoDimPheromoneSize - 1 - log.toInt
    val values = pheromone(edge)

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
    Array(res)
  }

  private def weightedCombination(edge: Edge): Array[Double] = {
    val weightedSum = false
    val values = pheromone(edge)
    //these values could be precalculated / cached (per iteration)
    val weightedValues = (0 until twoDimPheromoneSize).iterator.map { i =>
      val weight =
        if (i == twoDimPheromoneSize - 1) 1 / math.pow(2, i - 1)
        else 1 / math.pow(2, i)
      if (weightedSum) values(i) * weight else math.pow(values(i), weight)
    }
    val value = if (weightedSum) weightedValues.sum else weightedValues.product
    if (detailedDebug) println((value, values))
    Array(value)
  }

  private def pairingCombination(edge: Edge): Array[Double] = {
    val values = pheromone(edge)
    //these values could be precalculated / cached (per iteration)
    //pairing from outside to the center; within pairs calculate "final value" based on avg and diff
    // then the contrast between "positive" and "negative" values should be reinforced
    // finally calculate average of values got from pairs
    val value = (0 until twoDimPheromoneSize / 2).iterator.map { i =>
      val pos = values(i)
      val neg = values(twoDimPheromoneSize - i - 1)
      val v = ((pos + neg) / 2) + (pos - neg) * (twoDimPheromoneSize / 2 - i)
      if (detailedDebug) debug(v)

      //alternatives
//      ensureMinMax(v) //seems worse?
//      if (pos >= neg) 1.0 else 0.0 //too simple? ;)
      v
    }.sum / (twoDimPheromoneSize / 2)

    if (detailedDebug) debug((value, values))

    //additional adjustments?
    Array(ensureMinMax(value, minV = currentMin))
//    Array(value)
  }

  private def expectedCombination(edge: Edge): Array[Double] = {
    val values = pheromone(edge)
    val sum = values.sum
    // calculate "expected score" of the edge (between 0 and 1)
    val expectedValue = values.reverseIterator.zipWithIndex.map { case (v, ind) =>
      val partScore = (ind + 0.5) / twoDimPheromoneSize 
      v / sum * partScore // normalize v by sum and multiply by score 
    }.sum
    // adjust expected score between min and max
    val min = values.min
    val max = values.max
    val v = min + (max - min) * expectedValue
    if (detailedDebug) debug((expectedValue, v, values))
    Array(v)
  }

  override def pheromoneUpdate(solutionsRepo: BaseSolutionRepo): Unit = {
    cache.clear()
    
    val solutions = solutionsSelectionStrategy(solutionsRepo)
    val sortedSolutions = solutions.sortBy(_.evaluation.head).take(updateAnts.getOrElse(solutions.size))
    require(
      updateAnts.forall(_ == sortedSolutions.size),
      s"Wanted: $updateAnts update ants, got ${sortedSolutions.size} solutions"
    )

    val minCost = sortedSolutions.head.evaluation.head
    val maxCost = sortedSolutions.last.evaluation.head
    val partDiff = (maxCost - minCost) / twoDimPheromoneSize
    if (detailedDebug) debug(partDiff)

    def calcPartFromEvaluation(cost: Double): Int =
      ((cost - minCost) / partDiff).toInt.min(twoDimPheromoneSize - 1)
    def calcPartFromIndex(ind: Int): Int = ind * twoDimPheromoneSize / sortedSolutions.size
    if (detailedDebug) {
      debug(
        sortedSolutions
          .map(_.evaluation.head)
          .map(calcPartFromEvaluation)
          .groupBy(identity)
          .view
          .mapValues(_.size)
          .toList
          .sortBy(_._1)
      )
      debug(
        sortedSolutions.zipWithIndex
          .map(_._2)
          .map(calcPartFromIndex)
          .groupBy(identity)
          .view
          .mapValues(_.size)
          .toList
          .sortBy(_._1)
      )
    }

    debug(s"Updating pheromone with ${sortedSolutions.size} solutions - ${sortedSolutions.map(_.evaluation)}")
    sortedSolutions.zipWithIndex
      .groupBy { case (solution, ind) =>
        //both versions give acceptable results
        updateType match {
          case UpdateType.PartFromEvaluation =>
            calcPartFromEvaluation(solution.evaluation.head)
          case UpdateType.PartFromIndex =>
            calcPartFromIndex(ind)
        }
      }
      .foreach { case (part, partSolutions) =>
        val partIncrement = increment / partSolutions.size
        partSolutions.foreach { case (solution, _) =>
          solution.solution
            .sliding(2)
            .map(x => Edge(x.head, x.last))
            .foreach(edge =>
              pheromone.updateWith(edge)(pheromones =>
                pheromones.map(ph => ph.updated(part, ph(part) + partIncrement))
              )
            )
        }
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

    pheromone.mapValuesInPlace((_, values) =>
      values.map(extinctAndEnsureMinMax)
    )
    currentMin = extinctAndEnsureMinMax(currentMin)
    if (detailedDebug) {
      val values = pheromone.values.flatten
      debug((values.min, values.max, values.sum / values.size))
    }
  }

}
