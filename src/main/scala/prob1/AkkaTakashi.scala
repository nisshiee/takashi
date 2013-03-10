package org.nisshiee.takashi.prob1

import akka.actor._
import scalaz._, Scalaz._
import com.typesafe.config.ConfigFactory
import akka.routing.RoundRobinRouter
import Takashi._

object AkkaTakashi {

  lazy val system = ActorSystem("takashi-prob1")
  lazy val aggregater = system.actorOf(Props[Aggregater], "aggregater")
  lazy val searchActor =
    system.actorOf(Props[SearchActor].withRouter(RoundRobinRouter(4)), "searchActor")
  val depth = ConfigFactory.load.getInt("takashi.prob1.akka.search.depth")

  def routeCount(f: Field, id: Long) = {

    aggregater ! Aggregater.Start(id)
    val (goalCount, rest) = searchrec(f, depth)
    aggregater ! Aggregater.Push(id, rest.size - 1, goalCount)
    rest foreach { f => searchActor ! SearchActor.Req(f, id) }
  }

  def searchrec(f: Field, depth: Int): (Long, List[Field]) = {

    def next(f: Field, step: Point): (Long, List[Field]) = {
      val p = f.start + step
      f status p match {
        case Goal => 1L -> Nil
        case NotEnabled => 0L -> Nil
        case Enabled => searchrec(f.copy(f.enableds - f.start, p), depth - 1)
      }
    }

    depth match {
      case 0 => (0L, f :: Nil)
      case n => {
        List(Point(0, 1), Point(0, -1), Point(1, 0), Point(-1, 0)) map
        { p => next(f, p) } reduce
        (_ |+| _)
      }
    }
  }
}
