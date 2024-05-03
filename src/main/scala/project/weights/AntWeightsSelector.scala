package project.weights

trait AntWeightsSelector {
  def heuristicWeightsForIteration(i: Int): Array[Double]
  def pheromoneWeightsForIteration(i: Int): Array[Double]
}

object AntWeightsSelector {
  class Const(weights: Array[Double]) extends AntWeightsSelector {
    def heuristicWeightsForIteration(i: Int): Array[Double] = weights

    def pheromoneWeightsForIteration(i: Int): Array[Double] = weights
  }

  object D1 extends AntWeightsSelector {
    private val weights = Array(1.0)
    def heuristicWeightsForIteration(i: Int): Array[Double] = weights

    def pheromoneWeightsForIteration(i: Int): Array[Double] = weights

  }
}
