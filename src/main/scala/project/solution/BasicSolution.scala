package project.solution

import project.graph.Node

final case class BasicSolution(solution: Seq[Node], evaluation: IndexedSeq[Double]) extends BaseSolution(solution)
