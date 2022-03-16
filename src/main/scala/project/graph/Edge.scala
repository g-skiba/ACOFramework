package project.graph

case class Edge(node1: Node, node2: Node) {

  /** Cantor pairing function for optimized access of hash functions
    */
  override def hashCode: Int =
    (node1.number + node2.number) * (node1.number + node2.number + 1) / 2 + node2.number
}
