package org.nisshiee.takashi.prob3

case class Node(name: String)
case class Edge(n1: Node, n2: Node, distance: Int, fee: Int)
case class Graph(nodes: Set[Node], edges: Set[Edge], start: Node, goal: Node)

object Edge {

  trait EdgeInstance extends Distance[Edge] with Fee[Edge]

  implicit def EdgeInstance = new EdgeInstance {

    override def distance(a: Edge) = a.distance
    override def fee(a: Edge) = a.fee
  }
}
