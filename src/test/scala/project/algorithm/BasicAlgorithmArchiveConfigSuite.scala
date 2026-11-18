package project.algorithm

import pareto.Hypervolume2DCalculator
import project.config.{AlgorithmConfig, PheromoneConfig, SolutionsSelectionStrategy, TwoDimPheromoneConfig}
import project.config.PheromoneConfig.PheromoneType
import project.config.TwoDimPheromoneConfig.{GetType, UpdateType}
import project.graph.{Edge, Node}
import project.logging.BasicAcoLogger
import mokp.MultiObjectiveKnapsackData
import project.problem.{BaseProblem, MokpProblem}
import project.solution.{MokpSolution, SolutionUnderConstruction}

class BasicAlgorithmArchiveConfigSuite extends munit.FunSuite {
  private val silentLogger = new BasicAcoLogger("archive-wiring-test") {
    override protected def doPrint(message: String): Unit = ()
    override def close(): Unit = ()
  }

  // Each ant has one possible move, yielding four distinct solutions in two iterations.
  private class SequentialProblem(objectives: Int = 2) extends BaseProblem[Int](
    (0 to 4).map(Node(_)), (1 to 4).map(id => Edge(Node(0), Node(id))), Node(0), objectives, Seq.empty
  ) {
    private var nextItem = 0
    override protected def initState: Int = {
      nextItem += 1
      nextItem
    }
    override protected def updateState(state: Int, node: Node): Int = state
    override def getPossibleMoves(solution: SolutionUnderConstruction[Int]): collection.Set[Node] =
      if (solution.nodes.size == 1) Set(Node(solution.state)) else Set.empty[Node]
    override def getHeuristicValue(solution: SolutionUnderConstruction[Int], candidate: Node): Array[Double] =
      Array.fill(dimensions)(1.0)
    override def evaluate(solution: SolutionUnderConstruction[Int]): IndexedSeq[Double] =
      Vector.fill(dimensions)(solution.state.toDouble)
    override def getHypervolumeCalculator: Option[Hypervolume2DCalculator] = None
  }

  private def config(tableType: PheromoneType, mode: UpdateType, fronts: Int, history: Int): AlgorithmConfig =
    AlgorithmConfig(2, 2, 1.0, 1.0, PheromoneConfig(
      tableType.toString, -1, 0.1, 0.1, 0.001, 1.0, "ParetoFronts", 1,
      TwoDimPheromoneConfig(if (mode == UpdateType.PartFromParetoFront) 0 else 6, GetType.ExpectedCombination.toString, mode.toString),
      paretoFrontsCount = fronts, paretoHistoryLength = history
    ))

  test("wire the same front limit to the learning repository and both direct front table types") {
    for (tableType <- Vector(PheromoneType.TwoDim, PheromoneType.ItemTwoDim)) {
      val repo = new BasicAlgorithm(new SequentialProblem(), config(tableType, UpdateType.PartFromParetoFront, 4, 0), Some(0L))
        .run(silentLogger)
      assertEquals(repo.learningParetoFronts.map(_.head.evaluation.head), Vector(1.0, 2.0, 3.0, 4.0))
      assertEquals(repo.globalParetoSolutions.map(_.evaluation.head), Vector(1.0))
    }
  }

  test("wire admission history for Basic and 2D tables while retaining the reporting Pareto front") {
    for (tableType <- PheromoneType.values) {
      val repo = new BasicAlgorithm(new SequentialProblem(), config(tableType, UpdateType.PartFromIndex, 4, 1), Some(0L))
        .run(silentLogger)
      assertEquals(repo.learningParetoFronts.map(_.head.evaluation.head), Vector(3.0, 4.0))
      assertEquals(repo.globalParetoSolutions.map(_.evaluation.head), Vector(1.0))
    }
  }

  test("ants create the problem-specific completed solution type") {
    val problem = new MokpProblem(MultiObjectiveKnapsackData(
      objectives = Vector(Vector(1, 1, 1), Vector(1, 1, 1)),
      constraints = Vector(Vector(1, 1, 1)),
      capacities = Vector(2)
    ))
    val repo = new BasicAlgorithm(problem, config(PheromoneType.ItemTwoDim, UpdateType.PartFromIndex, 2, 0), Some(0L))
      .run(silentLogger)

    assertEquals(repo.solutionsForLastIteration.size, 2)
    assert(repo.solutionsForLastIteration.forall(_.isInstanceOf[MokpSolution]))
    repo.solutionsForLastIteration.foreach { solution =>
      assertEquals(solution.effectiveSolution, solution.solution.toSet: Iterable[Node])
    }
  }

  test("other selectors require zero fronts and keep learning archives disabled") {
    val cfg = config(PheromoneType.Basic, UpdateType.PartFromIndex, 0, 1)
    cfg.pheromoneConfig.solutionsSelectionStrategy = SolutionsSelectionStrategy.GlobalPareto.toString
    val repo = new BasicAlgorithm(new SequentialProblem(), cfg, Some(0L)).run(silentLogger)
    assertEquals(repo.globalParetoSolutions.map(_.evaluation.head), Vector(1.0))
    intercept[IllegalArgumentException](repo.learningParetoFronts)
    cfg.pheromoneConfig.paretoFrontsCount = 4
    intercept[IllegalArgumentException](new BasicAlgorithm(new SequentialProblem(), cfg, Some(0L)).run(silentLogger))
  }

  test("reject ParetoFronts for single-objective problems before running the colony") {
    val error = intercept[IllegalArgumentException] {
      new BasicAlgorithm(new SequentialProblem(1), config(PheromoneType.Basic, UpdateType.PartFromIndex, 2, 0), Some(0L))
        .run(silentLogger)
    }
    assert(error.getMessage.contains("single-objective"))
  }
}
