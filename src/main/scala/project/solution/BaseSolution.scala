package project.solution

import project.graph.Node

/** Completed solution. Its effective representation must stay immutable because its hash is cached. */
abstract class BaseSolution(final val effectiveSolution: Iterable[Node]) {
  def solution: Seq[Node]
  def evaluation: IndexedSeq[Double]

  private val effectiveSolutionHash: Int = effectiveSolution.hashCode

  final def isEquivalentTo(other: BaseSolution): Boolean =
    (this eq other) ||
      (effectiveSolutionHash == other.effectiveSolutionHash && effectiveSolution == other.effectiveSolution)
}

object BaseSolution {
  /** Construct the default solution, whose node order determines equivalence. */
  def apply(solution: Seq[Node], evaluation: IndexedSeq[Double]): BasicSolution =
    BasicSolution(solution, evaluation)
}
