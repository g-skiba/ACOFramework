package vrp

import java.io.File

import scala.collection.mutable
import scala.io.Source
import scala.util.control.Breaks

object VrpReader {

  def read(source: Source): Vrp = {
    var isReadingNODE_COORD_SECTION: Boolean = false
    var isReadingDEMAND_SECTION: Boolean = false
    var isReadingDEPOT_SECTION: Boolean = false

    var vrpName: Option[String] = None
    var vrpType: Option[VrpType] = None
    var depot: Option[CityName] = None
    var dimension: Option[Int] = None
    var capacity: Option[Int] = None
    var edgeWeightType: Option[EdgeWeightType] = None

    val nodeCoordSection: mutable.Map[CityName, (Double, Double)] =
      mutable.Map.empty
    val demandSection: mutable.Map[CityName, Int] =
      mutable.Map.empty

    val b: Breaks = new Breaks
    b.breakable {
      for (line <- source.getLines()) {
        line.trim match {
          case "NODE_COORD_SECTION" =>
            isReadingNODE_COORD_SECTION = true

          case "DEMAND_SECTION" =>
            isReadingDEMAND_SECTION = true
            isReadingNODE_COORD_SECTION = false

          case "DEPOT_SECTION" =>
            isReadingDEPOT_SECTION = true
            isReadingDEMAND_SECTION = false
          
          case "EOF" => b.break()

          case trimmed =>
            if (isReadingNODE_COORD_SECTION) {
              val Array(cityNameStr, xStr, yStr) = trimmed.split("\\s+")
              val cityName: CityName = CityName(cityNameStr)
              val x: Double = xStr.toDouble
              val y: Double = yStr.toDouble
              nodeCoordSection(cityName) = (x, y)

            } else if (isReadingDEMAND_SECTION) {
              val Array(cityNameStr, demandStr) = trimmed.split("\\s+")
              val cityName: CityName = CityName(cityNameStr)
              val demand: Int = demandStr.toInt
              demandSection(cityName) = demand

            } else if (isReadingDEPOT_SECTION) {
              depot = Some(CityName("1")) //Hard coded 1
            } else {
              val Array(nameNoChomp, valueNoChomp) = trimmed.split(":", 2)
              val name: String = nameNoChomp.trim
              val value: String = valueNoChomp.trim

              name match {
                case "NAME" =>
                  vrpName = Some(value)
                case "TYPE" =>
                  value match {
                    case "VRP" =>
                      vrpType = Some(VRPVrpType)
                    case "CVRP" =>
                      vrpType = Some(VRPVrpType)
                  }
                case "DIMENSION" =>
                  dimension = Some(value.toInt)
                case "EDGE_WEIGHT_TYPE" =>
                  value match {
                    case "EUC_2D" =>
                      edgeWeightType = Some(EUC_2D)
                  }
                case "CAPACITY" =>
                  capacity = Some(value.toInt)

                case "COMMENT" =>
                  System.err.println(s"$value")

                case _ =>
                  System.err.println(s"WARNING: Unsupported: $name")
                }
            }
          }
      }
    }

    Vrp(
      vrpName.get,
      vrpType.get,
      depot.get,
      capacity.get,
      dimension.get,
      edgeWeightType.get,
      demandSection.toMap,
      nodeCoordSection.toMap
    )
  }
}
