package org.nisshiee.takashi.prob1

import akka.actor._

class SearchActor extends Actor {

  import SearchActor._
  import Aggregater.Push

  def receive = {
    case Req(f, id) => {
      val goalCount = Takashi.routeCount(f)
      sender ! Aggregater.Push(id, -1L, goalCount)
    }
  }

}

object SearchActor {

  case class Req(f: Field, id: Long)

  def startSearchNode() = {
    import com.typesafe.config.ConfigFactory
    import scala.collection.JavaConverters._

    val actorNames = ConfigFactory.load.getStringList("takashi.prob1.akka.search.names").asScala
    val system = {
      val name = ConfigFactory.load.getString("takashi.prob1.akka.name")
      ActorSystem(name)
    }
    actorNames foreach { name =>
      system.actorOf(Props[SearchActor], name)
    }
  }
}
