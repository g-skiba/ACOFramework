package tsp

import java.io.File

import scala.collection.mutable
import scala.io.Source
import scala.util.control.Breaks

object TspReader {

  def read(source: Source): Tsp = {
    var isReadingNODE_COORD_SECTION: Boolean = false

    var tspName: Option[String] = None
    var tspType: Option[TspType] = None
    var dimension: Option[Int] = None
    var edgeWeightType: Option[EdgeWeightType] = None

    val nodeCoordSection: mutable.Map[CityName, (Double, Double)] =
      mutable.Map.empty

    val b: Breaks = new Breaks
    b.breakable {
      for (line <- source.getLines()) {
        if (isReadingNODE_COORD_SECTION) {
          line.trim match {
            case "EOF" => b.break()
            case trimmed =>
              val Array(cityNameStr, xStr, yStr) = trimmed.split("\\s+")
              val cityName: CityName = CityName(cityNameStr)
              val x: Double = xStr.toDouble
              val y: Double = yStr.toDouble
              nodeCoordSection(cityName) = (x, y)
          }
        } else {
          line.trim match {
            case "NODE_COORD_SECTION" =>
              isReadingNODE_COORD_SECTION = true
            case trimmed =>
              val Array(nameNoChomp, valueNoChomp) = trimmed.split(":")
              val name: String = nameNoChomp.trim
              val value: String = valueNoChomp.trim

              name match {
                case "NAME" =>
                  tspName = Some(value)
                case "TYPE" =>
                  value match {
                    case "TSP" =>
                      tspType = Some(TSPTspType)
                    case "ATSP" =>
                      tspType = Some(TSPTspType)
                  }
                case "DIMENSION" =>
                  dimension = Some(value.toInt)
                case "EDGE_WEIGHT_TYPE" =>
                  value match {
                    case "EUC_2D" =>
                      edgeWeightType = Some(EUC_2D)
                  }

                case _ =>
                  System.err.println(s"WARNING: Unsupported: $name")

              }
          }
        }
      }
    }

    Tsp(
      tspName.get,
      tspType.get,
      dimension.get,
      edgeWeightType.get,
      Map(nodeCoordSection.toSeq: _*)
    )
  }
}
