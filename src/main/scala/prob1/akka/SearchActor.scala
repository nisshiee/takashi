package org.nisshiee.takashi.prob1

import akka.actor._

class SearchActor extends Actor {

  import SearchActor._
  import Takashi._
  import Aggregater.{ Push => APush, DeadEnd => ADeadEnd, Goal => AGoal }

  val aggregater = context.actorFor("akka://takashi-prob1/user/aggregater")
  val searchActor = context.actorFor("akka://takashi-prob1/user/searchActor")

  def receive = {
    case Req(Field(_, s, g), id) if s == g => aggregater ! AGoal(id)
    case Req(f, id) => {
      aggregater ! APush(id)
      next(f, Point(0, -1), id)
      next(f, Point(0, 1), id)
      next(f, Point(-1, 0), id)
      next(f, Point(1, 0), id)
    }
  }

  def next(f: Field, step: Point, id: Long): Unit = {
    val p = f.start + step
    f status p match {
      case Goal => aggregater ! AGoal(id)
      case NotEnabled => aggregater ! ADeadEnd(id)
      case Enabled => searchActor ! Req(f.copy(f.enableds - f.start, p), id)
    }
  }
}

object SearchActor {

  case class Req(f: Field, id: Long)
}
