package project.repo

import mokp.MultiObjectiveKnapsackData
import project.graph.{Edge, Node}
import project.problem.{MokpProblem, Mtsp}
import project.solution.BaseSolution

class SolutionEquivalenceSuite extends munit.FunSuite {
  test("retain distinct tours with equal objective values") {
    val cities = (0 to 2).map(Node(_))
    val matrix = (for {
      from <- cities
      to <- cities
      if from != to
    } yield Edge(from, to) -> 1.0).toMap
    val mtsp = new Mtsp(cities, Seq(matrix, matrix))
    val tour1 = BaseSolution(Seq(Node(0), Node(1), Node(2)), Vector(3.0, 3.0))
    val tour2 = BaseSolution(Seq(Node(0), Node(2), Node(1)), Vector(3.0, 3.0))

    assert(!mtsp.areSolutionsEquivalent(tour1.solution, tour2.solution))
    val tourRepo = new ParetoSolutionRepo(mtsp.areSolutionsEquivalent)
    tourRepo.addSolutions(0, Vector(tour1, tour2))
    assert(tourRepo.paretoSolutionsForLastIteration == Vector(tour1, tour2))
    assert(tourRepo.globalParetoSolutions == Vector(tour1, tour2))
  }

  test("deduplicate reordered knapsack item sets while retaining distinct sets") {
    val mokp = new MokpProblem(MultiObjectiveKnapsackData(
      objectives = Vector(Vector(1, 1, 1), Vector(1, 1, 1)),
      constraints = Vector(Vector(1, 1, 1)),
      capacities = Vector(2)
    ))
    val items1 = BaseSolution(Seq(Node(3), Node(0), Node(1)), Vector(-2.0, -2.0))
    val itemsReordered = BaseSolution(Seq(Node(3), Node(1), Node(0)), Vector(-2.0, -2.0))
    val otherItems = BaseSolution(Seq(Node(3), Node(0), Node(2)), Vector(-2.0, -2.0))

    assert(mokp.areSolutionsEquivalent(items1.solution, itemsReordered.solution))
    assert(!mokp.areSolutionsEquivalent(items1.solution, otherItems.solution))
    val itemRepo = new ParetoSolutionRepo(mokp.areSolutionsEquivalent)
    itemRepo.addSolutions(0, Vector(items1, itemsReordered, otherItems))
    assert(itemRepo.paretoSolutionsForLastIteration == Vector(items1, otherItems))
    assert(itemRepo.globalParetoSolutions == Vector(items1, otherItems))
    itemRepo.addSolutions(1, Vector(itemsReordered))
    assert(itemRepo.globalParetoSolutions == Vector(items1, otherItems))
  }
}
