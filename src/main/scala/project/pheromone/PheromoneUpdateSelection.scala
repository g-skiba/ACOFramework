package project.pheromone

import project.config.SolutionsSelectionStrategy
import project.config.TwoDimPheromoneConfig.{GetType, UpdateType}
import project.solution.BaseSolution

/** Prepares update contributors independently of edge/item pheromone storage. */
private[pheromone] object PheromoneUpdateSelection {
  /** Keep group positions so Pareto front indices survive selection, including empty groups. */
  def selectForObjective(
      solutionGroups: IndexedSeq[IndexedSeq[BaseSolution]],
      dim: Int,
      updateAnts: Option[Int]
  ): IndexedSeq[IndexedSeq[BaseSolution]] = {
    val limit = updateAnts.filter(_ > 0)
    solutionGroups.map { group =>
      group.sortBy(_.evaluation(dim)).take(limit.getOrElse(group.size))
    }
  }

  /** Resolve the zero-size marker before allocating a table; front mapping uses the archive's configured limit. */
  def resolveBucketCount(
      configuredSize: Int,
      paretoFrontsCount: Int,
      selectionStrategy: SolutionsSelectionStrategy,
      updateType: UpdateType,
      getType: GetType
  ): Int = {
    val size = if (updateType == UpdateType.PartFromParetoFront) {
      require(configuredSize == 0,
        "twoDimSize must be zero for PartFromParetoFront; the bucket count is derived from paretoFrontsCount")
      paretoFrontsCount
    } else configuredSize
    validateBucketConfiguration(selectionStrategy, updateType, size, getType)
    size
  }

  def validateBucketConfiguration(
      selectionStrategy: SolutionsSelectionStrategy,
      updateType: UpdateType,
      bucketCount: Int,
      getType: GetType
  ): Unit = {
    require(bucketCount > 0, "The pheromone table must contain at least one bucket")
    require(
      getType != GetType.PairingCombination || bucketCount % 2 == 0,
      "PairingCombination requires an even pheromone bucket count"
    )
    require(
      updateType != UpdateType.PartFromParetoFront || selectionStrategy == SolutionsSelectionStrategy.ParetoFronts,
      "PartFromParetoFront requires the ParetoFronts solution selection strategy"
    )
  }

  def validateFrontCount(groupCount: Int, updateType: UpdateType, bucketCount: Int): Unit = {
    require(
      updateType != UpdateType.PartFromParetoFront || groupCount <= bucketCount,
      "The number of Pareto fronts must not exceed the number of pheromone buckets"
    )
  }

  /** Assign all selected contributors; Index/Evaluation do not apply another contributor limit. */
  def assignBuckets(
      selectedGroups: IndexedSeq[IndexedSeq[BaseSolution]],
      dim: Int,
      updateType: UpdateType,
      bucketCount: Int
  ): IndexedSeq[(BaseSolution, Int)] = updateType match {
    case UpdateType.PartFromParetoFront =>
      selectedGroups.zipWithIndex.flatMap { case (group, frontIndex) =>
        group.map(solution => (solution, frontIndex))
      }
    case UpdateType.PartFromEvaluation | UpdateType.PartFromIndex =>
      val sortedSolutions = selectedGroups.flatten.sortBy(_.evaluation(dim))
      if (sortedSolutions.isEmpty) IndexedSeq.empty
      else {
        val minCost = sortedSolutions.head.evaluation(dim)
        val maxCost = sortedSolutions.last.evaluation(dim)
        val partDiff = (maxCost - minCost) / bucketCount
        sortedSolutions.zipWithIndex.map { case (solution, index) =>
          val bucket = if (updateType == UpdateType.PartFromEvaluation) {
            if (partDiff == 0.0) 0
            else ((solution.evaluation(dim) - minCost) / partDiff).toInt.min(bucketCount - 1)
          } else index * bucketCount / sortedSolutions.size
          (solution, bucket)
        }
      }
  }
}
