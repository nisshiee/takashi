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

object Graph {

  def read(file: String): Graph = {
    import com.github.tototoshi.csv.CSVReader
    import scalaz._, Scalaz._

    val csv = CSVReader.open(file).toStream
    val empty = Graph(Set(), Set(), Node("たかし家前"), Node("市場前"))
    def foldfunc(g: Graph, record: List[String]): Graph = record match {
      case n1str :: n2str :: dstr :: fstr :: Nil => {
        val n1 = Node(n1str)
        val n2 = Node(n2str)
        val distance = dstr.parseInt | 0
        val fee = fstr.parseInt | 0

        val nodes = g.nodes + n1 + n2
        val edges = g.edges + Edge(n1, n2, distance, fee)
        g.copy(nodes = nodes, edges = edges)
      }
      case _ => g
    }

    csv.foldLeft(empty)(foldfunc)
  }
}
