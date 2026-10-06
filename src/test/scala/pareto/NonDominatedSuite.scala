package pareto

import project.graph.Node
import project.solution.BaseSolution

class NonDominatedSuite extends munit.FunSuite {
  private val evaluationCases: Vector[(String, IndexedSeq[IndexedSeq[Double]], IndexedSeq[Boolean])] = Vector(
    ("single objective: discard the worse value", Vector(Vector(1.0), Vector(2.0)), Vector(true, false)),
    ("single objective: retain the best value when it arrives second", Vector(Vector(2.0), Vector(1.0)), Vector(false, true)),
    ("single objective: discard a duplicate and a dominated value", Vector(Vector(1.0), Vector(1.0), Vector(2.0)), Vector(true, false, false)),
    ("single objective: keep one copy of equal values", Vector(Vector(1.0), Vector(1.0)), Vector(true, false)),

    ("first objective: discard the worse value", Vector(Vector(1.0, 0.0), Vector(2.0, 0.0)), Vector(true, false)),
    ("first objective: retain the best value when it arrives second", Vector(Vector(2.0, 0.0), Vector(1.0, 0.0)), Vector(false, true)),
    ("first objective: discard a duplicate and a dominated value", Vector(Vector(1.0, 0.0), Vector(1.0, 0.0), Vector(2.0, 0.0)), Vector(true, false, false)),
    ("first objective: keep one copy of equal values", Vector(Vector(1.0, 0.0), Vector(1.0, 0.0)), Vector(true, false)),

    ("second objective: discard the worse value", Vector(Vector(0.0, 1.0), Vector(0.0, 2.0)), Vector(true, false)),
    ("second objective: retain the best value when it arrives second", Vector(Vector(0.0, 2.0), Vector(0.0, 1.0)), Vector(false, true)),
    ("second objective: discard a duplicate and a dominated value", Vector(Vector(0.0, 1.0), Vector(0.0, 1.0), Vector(0.0, 2.0)), Vector(true, false, false)),
    ("second objective: keep one copy of equal values", Vector(Vector(0.0, 1.0), Vector(0.0, 1.0)), Vector(true, false)),

    ("retain both tradeoffs between objectives", Vector(Vector(1.0, 0.0), Vector(0.0, 2.0)), Vector(true, true)),
    ("retain both tradeoffs with the objective values reversed", Vector(Vector(2.0, 0.0), Vector(0.0, 1.0)), Vector(true, true))
  )

  for ((name, evaluations, expected) <- evaluationCases) {
    test(name) {
      assertEquals(getParetoFrontMin(evaluations)(identity), expected)
    }
  }

  test("discard a dominated solution with the same node sequence") {
    val solutions = IndexedSeq(
      BaseSolution(Seq(Node(1), Node(2)), IndexedSeq(1.0)),
      BaseSolution(Seq(Node(1), Node(2)), IndexedSeq(2.0))
    )
    assertEquals(getParetoFrontMin(solutions)(_.evaluation), IndexedSeq(true, false))
  }

  test("deduplicate identical solutions with equal objective values") {
    val solutions = IndexedSeq(
      BaseSolution(Seq(Node(1), Node(2)), IndexedSeq(1.0)),
      BaseSolution(Seq(Node(1), Node(2)), IndexedSeq(1.0))
    )
    assertEquals(getParetoFrontMin(solutions)(_.evaluation), IndexedSeq(true, false))
  }

  test("retain different node sequences with equal objective values") {
    val solutions = IndexedSeq(
      BaseSolution(Seq(Node(1), Node(2)), IndexedSeq(1.0)),
      BaseSolution(Seq(Node(1), Node(3)), IndexedSeq(1.0))
    )
    assertEquals(getParetoFrontMin(solutions)(_.evaluation), IndexedSeq(true, true))
  }

  test("use custom equivalence to deduplicate reordered node sets") {
    val solutions = IndexedSeq(
      BaseSolution(Seq(Node(1), Node(2)), IndexedSeq(1.0)),
      BaseSolution(Seq(Node(1), Node(3)), IndexedSeq(1.0)),
      BaseSolution(Seq(Node(3), Node(1)), IndexedSeq(1.0))
    )
    val selected = getParetoFrontMin(solutions)(_.evaluation, (first, second) => first.solution.toSet == second.solution.toSet)
    assertEquals(selected, IndexedSeq(true, true, false))
  }
}
