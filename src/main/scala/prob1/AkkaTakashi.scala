package org.nisshiee.takashi.prob1

import akka.actor._
import akka.routing.RoundRobinRouter

object AkkaTakashi {

  lazy val system = ActorSystem("takashi-prob1")
  lazy val aggregater = system.actorOf(Props[Aggregater], "aggregater")
  lazy val searchActor =
    system.actorOf(Props[SearchActor].withRouter(RoundRobinRouter(4)), "searchActor")

  def routeCount(f: Field, id: Long) = {
    aggregater ! Aggregater.Start(id)
    searchActor ! SearchActor.Req(f, id)
  }
}
