package pareto

import scala.annotation.tailrec

class Hypervolume2DCalculator(referencePoint: (Double, Double)) {
  private val refX: Double = referencePoint._1
  private val refY: Double = referencePoint._2
  def calculateFromUnsorted(paretoFront: Seq[IndexedSeq[Double]]): Double = {
    calculate(paretoFront.sortBy(s => s.apply(0)))
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

    calculate(0, refY, 0.0)
  }
}

object Hypervolume2DCalculator {
  def main(args: Array[String]): Unit = {
    val h1 = new Hypervolume2DCalculator((8, 9))
    println(h1.calculate(Vector(Vector(1, 7), Vector(3, 4), Vector(6,2))) == 33)
    println(h1.calculate(Vector(Vector(1, 7))) == 14)
  }
}