package project.pheromone

import project.graph.Edge
import project.solution.BaseSolution

import scala.collection.mutable.Map as MMap
import scala.util.Random

/** Assuming one-dimensional problem
 * consider:
 *  keep best (or generally more) solutions for pheromone update?
 *  range of solution cost (currently from iteration solutions)
 */
class TwoDimPheromone(
  edges: List[Edge],
  val increment: Double,
  val extinction: Double,
  val pheromoneDimension: Int = 10,
  minValue: Double = 0.001,
  maxValue: Double = 0.999,
  debug: Boolean = false
) extends BasePheromoneTable {
  require(pheromoneDimension % 2 == 0, "Temporary assumption for `getPheromone` based on pairing values starting from edges")

  val pheromone: MMap[Edge, IndexedSeq[Double]] =
    edges.map((_, IndexedSeq.fill(pheromoneDimension)(maxValue))).to(MMap)
  private var currentMin = maxValue

  override def getPheromone(edge: Edge): List[Double] = {
//    exponentialRandom(edge)
//    weightedCombination(edge)
    pairingCombination(edge)
  }

  private def exponentialRandom(edge: Edge): List[Double] = {
    val random = Random.nextInt((1 << pheromoneDimension) - 1) + 1
    val log = math.log(random) / math.log(2)
    val index = pheromoneDimension - 1 - log.toInt
    val values = pheromone(edge)
    if (debug) values(index)
    values(index) :: Nil
  }

  private def weightedCombination(edge: Edge): List[Double] = {
    val weightedSum = false
    val values = pheromone(edge)
    //these values could be precalculated / cached (per iteration)
    val weightedValues = (0 until pheromoneDimension).iterator.map { i =>
      val weight = if (i == pheromoneDimension - 1) 1 / math.pow(2, i - 1) else 1 / math.pow(2, i)
      if (weightedSum) values(i) * weight else math.pow(values(i), weight)
    }
    val value = if (weightedSum) weightedValues.sum else weightedValues.product
    if (debug) println((value, values))
    value :: Nil
  }

  private def pairingCombination(edge: Edge): List[Double] = {
    val values = pheromone(edge)
    //these values could be precalculated / cached (per iteration)
    //pairing from outside to the center; within pairs calculate "final value" based on avg and diff
    // then the contrast between "positive" and "negative" values should be reinforced
    // finally calculate average of values got from pairs
    val value = (0 until pheromoneDimension / 2).iterator.map { i =>
      val pos = values(i)
      val neg = values(pheromoneDimension - i - 1)
      val v = ((pos + neg) / 2) + (pos - neg) * (pheromoneDimension / 2 - i)
      if (debug) println(v)
      
      //alternatives
//      ensureMinMax(v)
//      if (pos >= neg) 1.0 else 0.0
      v
    }.sum / (pheromoneDimension / 2)
    
    if (debug) println((value, values))
    
    //additional adjustments?
    ensureMinMax(value).max(currentMin) :: Nil
//    value :: Nil
  }

  override def pheromoneUpdate(solutions: List[BaseSolution]): Unit = {
    val minCost = solutions.iterator.map(_.evaluation.head).min
    val maxCost = solutions.iterator.map(_.evaluation.head).max
    val partDiff = (maxCost - minCost) / pheromoneDimension
    if (debug) println(partDiff)

    def calcPartFromEvaluation(cost: Double): Int = ((cost - minCost) / partDiff).toInt.min(pheromoneDimension - 1)
    def calcPartFromIndex(ind: Int): Int = ind / pheromoneDimension
    if (debug) println(solutions.map(_.evaluation.head).map(calcPartFromEvaluation).groupBy(identity).view.mapValues(_.size).toList.sortBy(_._1))
    if (debug) println(solutions.zipWithIndex.map(_._2).map(calcPartFromIndex).groupBy(identity).view.mapValues(_.size).toList.sortBy(_._1))

    solutions.zipWithIndex
      .groupBy { case (solution, ind) =>
        //both versions give acceptable results
        calcPartFromEvaluation(solution.evaluation.head)
//        calcPartFromIndex(ind)
      }
      .foreach { case (part, partSolutions) =>
        val partIncrement = increment / partSolutions.size
        partSolutions.foreach { case (solution, _) =>
          solution.solution
            .sliding(2)
            .map(x => Edge(x.head, x.last))
            .foreach(edge =>
              pheromone.updateWith(edge)(pheromones => pheromones.map(ph => ph.updated(part, ph(part) + partIncrement)))
            )
        }
      }
  }

  private def ensureMinMax(double: Double): Double = {
    double.min(maxValue).max(minValue)
  }

  override def afterUpdatesAction(): Unit = {
    def extinctAndEnsureMinMax(double: Double): Double = {
      val e = double * (1 - extinction)
      ensureMinMax(e)
    }

    pheromone.mapValuesInPlace((_, values) => values.map(extinctAndEnsureMinMax))
    currentMin = extinctAndEnsureMinMax(currentMin)
    if (debug) {
      val values = pheromone.values.flatten
      println((values.min, values.max, values.sum / values.size))
    }
  }

}