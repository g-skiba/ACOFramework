package pareto

import scala.annotation.tailrec

/**
 * @param maxReferencePoint overestimation of the possible cost for all objectives, used to calculate surface between
 *                          pareto front and this point
 * @param minReferencePoint underestimation of the possible cost for all objectives, used to calculate reference surface
 *                          and therefore change hypervolume to a value between 0 and 1
 */
class Hypervolume2DCalculator(maxReferencePoint: (Double, Double), minReferencePoint: (Double, Double) = (0, 0)) {
  private val refX: Double = maxReferencePoint._1
  private val refY: Double = maxReferencePoint._2
  private val refSurface: Double = (refX - minReferencePoint._1) * (refY - minReferencePoint._2)
  def calculateFromUnsorted(paretoFront: Seq[IndexedSeq[Double]]): Double = {
    calculate(paretoFront.sortBy(s => s(0)))
  }

  def calculate(paretoFrontSorted: Seq[IndexedSeq[Double]]): Double = {
    require(paretoFrontSorted.forall(_.size == 2))
    require(paretoFrontSorted.forall(s => s(0) < refX && s(1) < refY))
    require(paretoFrontSorted.sliding(2).forall {
      case Seq(IndexedSeq(x1, y1), IndexedSeq(x2, y2)) => x1 <= x2 && y1 >= y2
      case _ => true
    })

    @tailrec
    def calculate(ind: Int, curRefY: Double, acc: Double): Double = {
      if (ind >= paretoFrontSorted.size) acc
      else {
        val curPoint = paretoFrontSorted(ind)
        val xDiff = refX - curPoint(0)
        val yDiff = curRefY - curPoint(1)
        assert(xDiff > 0 && yDiff > 0)
        calculate(ind + 1, curPoint(1), acc + xDiff * yDiff)
      }
    }

    calculate(0, refY, 0.0)
  }

  /**
   * Can be used for minimisation
   */
  def calculateRemainingPartFromUnsorted(paretoFront: Seq[IndexedSeq[Double]]): Double = {
    val surface = calculateFromUnsorted(paretoFront)
    1.0 - surface / refSurface
  }
}

object Hypervolume2DCalculator {
  def main(args: Array[String]): Unit = {
    val h1 = new Hypervolume2DCalculator((8, 9))
    println(h1.calculate(Vector(Vector(1, 7), Vector(3, 4), Vector(6,2))) == 33)
    println(h1.calculateRemainingPartFromUnsorted(Vector(Vector(3, 4), Vector(1, 7), Vector(6,2))) == 1.0 - 33.0 / 72.0)
    println(h1.calculate(Vector(Vector(1, 7))) == 14)
    println(h1.calculateRemainingPartFromUnsorted(Vector(Vector(1, 7))) == 1.0 - 14.0 / 72.0)
    val h2 = new Hypervolume2DCalculator((8, 9), (-1, -1))
    println(h2.calculate(Vector(Vector(1, 7), Vector(3, 4), Vector(6,2))) == 33.0)
    println(h2.calculateRemainingPartFromUnsorted(Vector(Vector(6,2), Vector(1, 7), Vector(3, 4))) == 1.0 - 33.0 / 90.0)
    println(h2.calculate(Vector(Vector(1, 7))) == 14.0)
    println(h2.calculateRemainingPartFromUnsorted(Vector(Vector(1, 7))) == 1.0 - 14.0 / 90.0)
  }
}