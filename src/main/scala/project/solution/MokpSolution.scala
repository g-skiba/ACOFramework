package project.solution

import project.graph.Node

final case class MokpSolution(solution: Seq[Node], evaluation: IndexedSeq[Double]) extends BaseSolution(solution.toSet)
