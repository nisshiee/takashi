package org.nisshiee.takashi.prob3

import scala.annotation.tailrec
import scalaz._, Scalaz._

case class Route(steps: List[Node], cost: Int)

object Takashi {

  def minDistanceRoute(g: Graph) = minCostRoute[Tags.Distance](g)
  def minFeeRoute(g: Graph) = minCostRoute[Tags.Fee](g)

  def minCostRoute[T](g: Graph)(implicit c: Cost[Edge @@ T]): Route =
    rec[T](g, Set(Route(g.goal :: Nil, 0)))

  @tailrec
  def rec[T](g: Graph, decided: Set[Route])(implicit c: Cost[Edge @@ T]): Route =
    decided find { _.steps.head == g.start } match {
      case Some(r) => r
      case None => {
        val unreached = g.nodes -- (decided map (_.steps.head))
        val candidates = decided flatMap {
          case Route(decidedSteps, decidedCost) =>
            neighbors[T](g, decidedSteps.head) filter {
              case (n, _) => unreached contains n
            } map {
              case (n, c) => Route(n :: decidedSteps, decidedCost + c)
            }
        }
        val next = decided + candidates.minBy(_.cost)
        rec[T](g, next)
      }
    }

  def neighbors[T](g: Graph, n: Node)(implicit c: Cost[Edge @@ T]): Set[(Node, Int)] =
    g.edges collect {
      case e @ Edge(n1, n2, _, _) if n1 == n => n2 -> Tag[Edge, T](e).cost
      case e @ Edge(n1, n2, _, _) if n2 == n => n1 -> Tag[Edge, T](e).cost
    }
}

