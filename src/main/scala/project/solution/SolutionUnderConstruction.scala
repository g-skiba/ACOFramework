package project.solution

import project.graph.Node

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

// using mutable structure for improved performance
case class SolutionUnderConstruction[T](nodes: ArrayBuffer[Node], state: T)

// using mutable structures for improved performance
case class TspState(nodesToVisit: mutable.Set[Node])
case class VrpState(nodesToVisit: mutable.Set[Node], remainingCapacity: Int, vehicles: Int)
