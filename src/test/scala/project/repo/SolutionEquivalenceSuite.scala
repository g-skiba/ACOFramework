package project.repo

import mokp.MultiObjectiveKnapsackData
import project.graph.{Edge, Node}
import project.problem.{MokpProblem, Mtsp}
import project.solution.BaseSolution

class SolutionEquivalenceSuite extends munit.FunSuite {
  private def mokpData = MultiObjectiveKnapsackData(
    objectives = Vector(Vector(1, 1, 1), Vector(1, 1, 1)),
    constraints = Vector(Vector(1, 1, 1)),
    capacities = Vector(2)
  )

  test("retain distinct tours with equal objective values") {
    val cities = (0 to 2).map(Node)
    val matrix = (for {
      from <- cities
      to <- cities
      if from != to
    } yield Edge(from, to) -> 1.0).toMap
    val mtsp = new Mtsp(cities, Seq(matrix, matrix))
    val tour1 = mtsp.createSolution(Seq(Node(0), Node(1), Node(2)), Vector(3.0, 3.0))
    val tour2 = mtsp.createSolution(Seq(Node(0), Node(2), Node(1)), Vector(3.0, 3.0))

    assert(!tour1.isEquivalentTo(tour2))
    val tourRepo = new ParetoSolutionRepo()
    tourRepo.addSolutions(0, Vector(tour1, tour2))
    assert(tourRepo.paretoSolutionsForLastIteration == Vector(tour1, tour2))
    assert(tourRepo.globalParetoSolutions == Vector(tour1, tour2))
  }

  test("deduplicate reordered knapsack item sets while retaining distinct sets") {
    val mokp = new MokpProblem(mokpData)
    val items1 = mokp.createSolution(Seq(Node(3), Node(0), Node(1)), Vector(-2.0, -2.0))
    val itemsReordered = mokp.createSolution(Seq(Node(3), Node(1), Node(0)), Vector(-2.0, -2.0))
    val otherItems = mokp.createSolution(Seq(Node(3), Node(0), Node(2)), Vector(-2.0, -2.0))

    assert(items1.isEquivalentTo(itemsReordered))
    assert(!items1.isEquivalentTo(otherItems))
    val itemRepo = new ParetoSolutionRepo()
    itemRepo.addSolutions(0, Vector(items1, itemsReordered, otherItems))
    assert(itemRepo.paretoSolutionsForLastIteration == Vector(items1, otherItems))
    assert(itemRepo.globalParetoSolutions == Vector(items1, otherItems))
    itemRepo.addSolutions(1, Vector(itemsReordered))
    assert(itemRepo.globalParetoSolutions == Vector(items1, otherItems))
  }

  test("compute each effective representation once across reporting and learning archive comparisons") {
    var representationsBuilt = 0
    def countedSolution(nodes: Seq[Node]): BaseSolution = new BaseSolution({
      representationsBuilt += 1
      nodes.toSet
    }) {
      override val solution: Seq[Node] = nodes
      override val evaluation: IndexedSeq[Double] = Vector(-2.0, -2.0)
    }
    val original = countedSolution(Vector(Node(3), Node(0), Node(1)))
    val reordered = countedSolution(Vector(Node(3), Node(1), Node(0)))
    val distinct = countedSolution(Vector(Node(3), Node(0), Node(2)))
    val repo = new ParetoSolutionRepo(paretoFrontsCount = 2)

    assertEquals(representationsBuilt, 3)
    for (iteration <- 0 until 5) {
      repo.addSolutions(iteration, Vector(original, reordered, distinct))
      assertEquals(repo.globalParetoSolutions, Vector(original, distinct))
      assertEquals(repo.learningParetoFronts, Vector(Vector(original, distinct)))
    }
    assertEquals(representationsBuilt, 3)
  }

  test("snapshot construction nodes and attach the problem-specific representation when completing a solution") {
    val mokp = new MokpProblem(mokpData)
    val construction = mokp.updateSolution(mokp.initSolution, Node(0))
    val completed = mokp.completeSolution(construction)
    mokp.updateSolution(construction, Node(1))

    assertEquals(completed.solution, Vector(Node(3), Node(0)))
    assertEquals(completed.effectiveSolution, Set(Node(3), Node(0)): Iterable[Node])
    assertEquals(completed.evaluation, Vector(-1.0, -1.0))
  }

  test("copies rebuild their effective representation for changed nodes") {
    val mokp = new MokpProblem(mokpData)
    val original = mokp.createSolution(Vector(Node(3), Node(0), Node(1)), Vector(-2.0, -2.0))
    val reordered = original.copy(solution = original.solution.reverse)
    val distinct = original.copy(solution = Vector(Node(3), Node(0), Node(2)))

    assert(original.isEquivalentTo(reordered))
    assert(!original.isEquivalentTo(distinct))
    val ordered = BaseSolution(original.solution, original.evaluation)
    assert(!ordered.isEquivalentTo(ordered.copy(solution = ordered.solution.reverse)))
  }

  test("retain set semantics for reordered sequences containing duplicate items") {
    val mokp = new MokpProblem(mokpData)
    val original = mokp.createSolution(Vector(Node(3), Node(0), Node(1)), Vector(-2.0, -2.0))
    val repeated = mokp.createSolution(Vector(Node(1), Node(3), Node(0), Node(0)), Vector(-2.0, -2.0))

    assert(original.isEquivalentTo(repeated))
  }

  test("hash collisions do not make different solutions equivalent") {
    for (dimensions <- Vector(1, 2, 3)) {
      var hashesComputed = 0
      def collidingSolution(nodes: Seq[Node]): BaseSolution = new BaseSolution(
        new scala.collection.immutable.AbstractSeq[Node] {
          override def length: Int = nodes.length
          override def apply(index: Int): Node = nodes(index)
          override def iterator: Iterator[Node] = nodes.iterator
          override def hashCode: Int = {
            hashesComputed += 1
            0
          }
          }
      ) {
        override val solution: Seq[Node] = nodes
        override val evaluation: IndexedSeq[Double] = Vector.fill(dimensions)(1.0)
        }
      val first = collidingSolution(Vector(Node(0)))
      val second = collidingSolution(Vector(Node(1)))
      val duplicate = collidingSolution(Vector(Node(0)))
      val repo = new ParetoSolutionRepo(paretoFrontsCount = 1, dimensions = dimensions)
  
      assertEquals(hashesComputed, 3)
      for (iteration <- 0 until 5) {
        repo.addSolutions(iteration, Vector(first, second, duplicate))
        assertEquals(repo.globalParetoSolutions, Vector(first, second))
        assertEquals(repo.learningParetoFronts.map(_.toSet), Vector(Set(first, second)))
        assertEquals(repo.learningParetoFronts.map(_.size), Vector(2))
      }
      assertEquals(hashesComputed, 3)
    }
  }
}
