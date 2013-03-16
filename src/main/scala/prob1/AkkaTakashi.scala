package org.nisshiee.takashi.prob1

import akka.actor._
import scalaz._, Scalaz._
import com.typesafe.config.ConfigFactory
import akka.routing._
import Takashi._
import scala.collection.JavaConverters._

object AkkaTakashi {

  lazy val system = ActorSystem("takashi-prob1")
  lazy val aggregater = system.actorOf(Props[Aggregater], "aggregater")
  val depth = ConfigFactory.load.getInt("takashi.prob1.akka.search.depth")
  val searchActorPath = ConfigFactory.load.getString("takashi.prob1.akka.searchActor")
  val searchActorNodes = ConfigFactory.load.getStringList("takashi.prob1.akka.searchActorNodes").asScala

  //def startSearchNode = system.actorOf(Props[SearchActor].withRouter(FromConfig()), "searchActor")
  def startSearchNode = {
    system.actorOf(Props[SearchActor], "searchActor01")
    system.actorOf(Props[SearchActor], "searchActor02")
    system.actorOf(Props[SearchActor], "searchActor03")
    system.actorOf(Props[SearchActor], "searchActor04")
  }

  def routeCount(f: Field, id: Long) = {

    //val searchActor = system.actorFor(searchActorPath)

    val router = new RoundRobinRouter(routees = searchActorNodes)
    val searchActor = system.actorOf(Props[SearchActor].withRouter(router), "searchActor")

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
