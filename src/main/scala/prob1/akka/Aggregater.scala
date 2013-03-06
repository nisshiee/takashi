package org.nisshiee.takashi.prob1

import akka.actor._

class Aggregater extends Actor {

  import Aggregater._

  private[this] val state = scala.collection.mutable.Map[Long, State]()

  override def receive = {
    case Start(id) => start(id)
    case Push(id) => stateChange(id, 3L, 0L)
    case DeadEnd(id) => stateChange(id, -1L, 0L)
    case Goal(id) => stateChange(id, -1L, 1L)
  }

  def start(id: Long) = {
    state += (id -> State(1L, 0L))
    ()
  }

  def stateChange(id: Long, stack: Long, goal: Long) = state.get(id) foreach {
    case State(s, g) => {
      val newState = State(s + stack, g + goal)
      if (newState.stack == 0) {
        println(s"result = ${ newState.goal }")
        state -= id
      } else {
        state += (id -> newState)
      }
      ()
    }
  }

}

object Aggregater {

  case class Start(id: Long)
  case class Push(id: Long)
  case class DeadEnd(id: Long)
  case class Goal(id: Long)

  case class State(stack: Long, goal: Long)
}
