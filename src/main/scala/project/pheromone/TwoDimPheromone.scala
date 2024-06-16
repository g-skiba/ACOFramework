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
  require(
    twoDimPheromoneSize % 2 == 0,
    "Temporary assumption for `getPheromone` based on pairing values starting from edges"
  )
  private val maxCantorValue = edges.iterator.map(e => e.cantorValue).max
  // indexed by edges' cantor value
  private val cache: ArrayCache[Array[Double]] = new ArrayCache[Array[Double]](maxCantorValue, null)

  // indexed by dim and edges' cantor value
  val pheromone: Array[Array[Array[Double]]] =
    Array.fill(pheromoneDimension)(Array.fill(maxCantorValue + 1)(Array.fill(twoDimPheromoneSize)(maxValue)))
  private var currentMin = maxValue

  override def getPheromone(edge: Edge): Array[Double] = {
    def calculate = {
      Array.tabulate[Double](pheromoneDimension) { dim =>
        val phValues = pheromone(dim)(edge.cantorValue)

        getType match {
          case GetType.ExponentialRandom => exponentialRandom(phValues, maxUpTo = false)
          case GetType.ExponentialRandomMax => exponentialRandom(phValues, maxUpTo = true)
          case GetType.WeightedCombination => weightedCombination(phValues)
          case GetType.PairingCombination => pairingCombination(phValues)
          case GetType.ExpectedCombination => expectedCombination(phValues)
        }
      }
    }

    if (getType.isRandomized) calculate
    else cache.getOrElseUpdate(edge.cantorValue, calculate)
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
    //these values could be precalculated / cached (per iteration)
    val weightedValues = (0 until twoDimPheromoneSize).iterator.map { i =>
      val weight =
        if (i == twoDimPheromoneSize - 1) 1 / math.pow(2, i - 1)
        else 1 / math.pow(2, i)
      if (weightedSum) values(i) * weight else math.pow(values(i), weight)
    }
    val value = if (weightedSum) weightedValues.sum else weightedValues.product
    if (detailedDebug) println((value, values))
    value
  }

  private def pairingCombination(values: Array[Double]): Double = {
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
    ensureMinMax(value, minV = currentMin)
//    value
  }

  private def expectedCombination(values: Array[Double]): Double = {
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
    v
  }

  override def pheromoneUpdate(solutionsRepo: BaseSolutionRepo): Unit = {
    cache.clear()

    def updateDim(dim: Int, sortedSolutions: IndexedSeq[BaseSolution]): Unit = {
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
              .foreach { edge =>
                val pheromones = pheromone(dim)(edge.cantorValue)
                pheromones(part) += increment
              }
          }
        }
    }

    val solutions = solutionsSelectionStrategy(solutionsRepo)

    def sortedSolutions(dim: Int): IndexedSeq[BaseSolution] = {
      solutions.sortBy(_.evaluation(dim))
    }

    val takeSolutions = updateAnts.getOrElse(solutions.size)
    pheromoneDimension match {
      case 1 =>
        updateDim(0, sortedSolutions(0).take(takeSolutions))
      case 2 =>
        val sorted = sortedSolutions(0)
        updateDim(0, sorted.take(takeSolutions))
        updateDim(1, sorted.reverseIterator.take(takeSolutions).toIndexedSeq)
      case _ =>
        for (dim <- 0 until pheromoneDimension) {
          val sorted = sortedSolutions(dim)
          updateDim(dim, sorted.take(takeSolutions))
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

    pheromone.foreach(_.foreach(_.mapInPlace(extinctAndEnsureMinMax)))
    currentMin = extinctAndEnsureMinMax(currentMin)
    if (detailedDebug) {
      pheromone.foreach { p =>
        val values = p.flatten
        debug((values.min, values.max, values.sum / values.size))
      }
    }
  }

}
