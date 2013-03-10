package org.nisshiee.takashi.prob1

import akka.actor._

class Aggregater extends Actor {

  import Aggregater._

  private[this] val state = scala.collection.mutable.Map[Long, State]()

  override def receive = {
    case Start(id) => start(id)
    case Push(id, stack, goal) => stateChange(id, stack, goal)
  }

  def start(id: Long) = {
    state += (id -> State(1L, 0L, System.currentTimeMillis))
    ()
  }

  def stateChange(id: Long, stack: Long, goal: Long) = state.get(id) foreach {
    case State(s, g, startTime) => {
      val newState = State(s + stack, g + goal, startTime)
      if (newState.stack == 0) {
        val endTime = System.currentTimeMillis
        println(s"result = ${ newState.goal }")
        println(s"time = ${ endTime - startTime } ms")
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
  case class Push(id: Long, stack: Long, goal: Long)

  case class State(stack: Long, goal: Long, startTime: Long)
}
