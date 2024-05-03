package pareto

import scala.annotation.tailrec

/**
 * @param referencePoint overestimation of the possible cost for all objectives, used to calculate surface between
 *                       pareto front and this point
 * @param surfacePartToAxes if all costs are always positive we can normalize the surface to value between 0 and 1 as
 *                          in a fraction of the rectangle determined by referencePoint and axes
 */
class Hypervolume2DCalculator(referencePoint: (Double, Double), surfacePartToAxes: Boolean) {
  private val refX: Double = referencePoint._1
  private val refY: Double = referencePoint._2
  def calculateFromUnsorted(paretoFront: Seq[IndexedSeq[Double]]): Double = {
    calculate(paretoFront.sortBy(s => s(0)))
  }

  def calculate(paretoFrontSorted: Seq[IndexedSeq[Double]]): Double = {
    require(paretoFrontSorted.forall(_.size == 2))
    require(paretoFrontSorted.forall(s => s(0) < refX && s(1) < refY))
    require(paretoFrontSorted.sliding(2).forall {
      case Seq(IndexedSeq(x1, y1), IndexedSeq(x2, y2)) => x1 < x2 && y1 > y2
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

    val surface = calculate(0, refY, 0.0)
    if (surfacePartToAxes) surface / (refX * refY) else surface
  }
}

object Hypervolume2DCalculator {
  def main(args: Array[String]): Unit = {
    val h1 = new Hypervolume2DCalculator((8, 9), surfacePartToAxes = false)
    println(h1.calculate(Vector(Vector(1, 7), Vector(3, 4), Vector(6,2))) == 33)
    println(h1.calculate(Vector(Vector(1, 7))) == 14)
    val h2 = new Hypervolume2DCalculator((8, 9), surfacePartToAxes = true)
    println(h2.calculate(Vector(Vector(1, 7), Vector(3, 4), Vector(6,2))) == 33.0 / 72.0)
    println(h2.calculate(Vector(Vector(1, 7))) == 14.0 / 72.0)
  }
}