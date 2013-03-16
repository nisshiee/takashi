package org.nisshiee.takashi.prob1

import akka.actor._
import com.typesafe.config.ConfigFactory

class SearchActor extends Actor {

  import SearchActor._
  import Aggregater.Push

  val aggregaterPath = ConfigFactory.load.getString("takashi.prob1.akka.aggregater")
  //val aggregater = context.actorFor("akka://takashi-prob1/user/aggregater")

  val aggregater = context.actorFor(aggregaterPath)

  def receive = {
    case Req(f, id) => {
      println("receive!")
      val goalCount = Takashi.routeCount(f)
      println("finish!")
      aggregater ! Aggregater.Push(id, -1L, goalCount)
    }
  }

}

object SearchActor {

  case class Req(f: Field, id: Long)
}
