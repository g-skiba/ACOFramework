package vrp

import project.graph.Node
import project.problem.VrpProblem
import tsp.nodeCoordSectionToMatrix

sealed trait VrpType
case object VRPVrpType extends VrpType

sealed trait EdgeWeightType
case object EUC_2D extends EdgeWeightType

case class CityName(value: String) extends Ordered[CityName]{
    def compare(that: CityName) = (this.value) compare (that.value)
}
// case class CityName(value: String) extends AnyVal

// object CityName{
//     val alphabetOrder = Ordering.by{(foo: CityName) => foo.value}
// }

case class Vrp(
    name: String,
    ty: VrpType,
    depot: CityName,
    capacity: Int,
    dimension: Int,
    edgeWeightType: EdgeWeightType,
    demandSection: Map[CityName, Int],
    nodeCoordSection: Map[CityName, (Double, Double)]
)
