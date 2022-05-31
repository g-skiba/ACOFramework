package vrp
import vrp.Vrp
import project.graph.Node
import project.problem.VrpProblem
import project.graph.Edge
import tsp.nodeCoordSectionToMatrix
import tsp.euclideanDistance

def VrpToProblem(vrp: Vrp): (Map[Node, CityName], VrpProblem) = {
  val citiesEnumerated = vrp.nodeCoordSection
    .keys
    .zipWithIndex
    .map((name, index) => (name, Node(index)))
    .toMap
 
  val mapped = vrp.nodeCoordSection.map(keyValue =>
    (citiesEnumerated(keyValue._1), keyValue._2)
  )

  val mappedDemands = vrp.demandSection.map(keyValue =>
    (citiesEnumerated(keyValue._1), keyValue._2)
  )

  val depot = citiesEnumerated(vrp.depot)

  val vrpProblem = VrpProblem(
    capacity = vrp.capacity,
    depot = depot,
    nodes = mapped.keys.toList,
    matrix = nodeCoordSectionToMatrix(mapped),
    demands = mappedDemands
  )

  (citiesEnumerated.map(_.swap).toMap, vrpProblem)
}