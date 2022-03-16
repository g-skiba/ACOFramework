package tsp
import tsp.Tsp
import project.graph.Node
import project.problem.TspProblem
import tsp.nodeCoordSectionToMatrix

def TspToProblem(tsp: Tsp): (Map[Node, CityName], TspProblem) = {
  val citiesEnumerated = tsp.nodeCoordSection
    .keys
    .zipWithIndex
    .map((name, index) => (name, Node(index)))
    .toMap

  val mapped = tsp.nodeCoordSection.map(keyValue =>
    (citiesEnumerated(keyValue._1), keyValue._2)
  )

  val tspProblem = TspProblem(
    nodes = mapped.keys.toList,
    matrix = nodeCoordSectionToMatrix(mapped)
  )
  (citiesEnumerated.map(_.swap).toMap, tspProblem)
}
