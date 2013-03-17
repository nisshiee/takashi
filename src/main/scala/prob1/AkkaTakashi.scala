package org.nisshiee.takashi.prob1

import akka.actor._
import com.typesafe.config.ConfigFactory
import akka.routing._
import scala.collection.JavaConverters._

class AkkaTakashi extends Actor {

  override def receive = {
    case (aggregater: ActorRef, mes: Aggregater.Start) => aggregater ! mes
    case Aggregater.Result(count, time) => {
      println(s"result = $count")
      println(s"time = $time ms")
      //context.system.shutdown
      System.exit(0)
    }
  }
}

object AkkaTakashi {

  lazy val system = ActorSystem(ConfigFactory.load().getString("takashi.prob1.akka.name"))
  lazy val initSearchDepth = ConfigFactory.load().getInt("takashi.prob1.initSearchDepth")
  lazy val aggregater = system.actorFor {
    ConfigFactory.load().getString("takashi.prob1.aggregaterActor")
  }
  lazy val search = {
    val actors = ConfigFactory.load().getStringList("takashi.prob1.searchActors").asScala
    val router = new RoundRobinRouter(routees = actors)
    system.actorOf(Props[SearchActor].withRouter(router), "aggregater")
  }
  lazy val takashi = system.actorOf(Props[AkkaTakashi], "takashi")

  def routeCount(f: Field) =
    takashi ! (aggregater, Aggregater.Start(f, initSearchDepth, search))
}
