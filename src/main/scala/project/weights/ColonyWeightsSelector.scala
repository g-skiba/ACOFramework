package project.weights

trait ColonyWeightsSelector {
  def weightsSelectorForAnt(i: Int): AntWeightsSelector
}

object ColonyWeightsSelector {
  object D2 {
    class Uniform(minWeight: Double, maxWeight: Double, antsNum: Int) extends ColonyWeightsSelector {
      private val range = maxWeight - minWeight
      private val part: Double = range / (antsNum - 1)

      def weightsSelectorForAnt(i: Int): AntWeightsSelector = {
        val firstWeight = minWeight + i * part
        val weights = Array(firstWeight, 1.0 - firstWeight)
        new AntWeightsSelector.Const(weights)
      }
    }
  }

  object D1 extends ColonyWeightsSelector {
    def weightsSelectorForAnt(i: Int): AntWeightsSelector = AntWeightsSelector.D1
  }
}
