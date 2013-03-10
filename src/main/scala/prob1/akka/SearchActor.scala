package org.nisshiee.takashi.prob1

import akka.actor._

class SearchActor extends Actor {

  import SearchActor._
  import Aggregater.Push

  val aggregater = context.actorFor("akka://takashi-prob1/user/aggregater")
  val searchActor = context.actorFor("akka://takashi-prob1/user/searchActor")

  def receive = {
    case Req(f, id) => {
      val goalCount = Takashi.routeCount(f)
      aggregater ! Aggregater.Push(id, -1L, goalCount)
    }
  }

}

object SearchActor {

  case class Req(f: Field, id: Long)
}
