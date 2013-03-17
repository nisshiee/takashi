package org.nisshiee.takashi.prob1

import akka.actor._

class Aggregater extends Actor {

  import Aggregater._

  private[this] val states = scala.collection.mutable.Map[Long, State]()
  private[this] var nextId = 1L

  override def receive = {
    case Start(f, d, s) => start(f, d, s)
    case Push(id, stack, goal) => stateChange(id, stack, goal)
  }

  def start(field: Field, initSearchDepth: Int, search: ActorRef) = {
    val startTime = System.currentTimeMillis
    val (goalCount, rest) = searchrec(field, initSearchDepth)
    val id = nextId
    nextId += 1L
    states += (id -> State(rest.size, goalCount, startTime, sender))
    rest foreach { f => search ! SearchActor.Req(f, id) }
  }

  def searchrec(f: Field, depth: Int): (Long, List[Field]) = {

    import Takashi._
    import scalaz._ ,Scalaz._

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

  def stateChange(id: Long, stack: Long, goal: Long) = states.get(id) foreach {
    case State(s, g, startTime, sender) => {
      val newState = State(s + stack, g + goal, startTime, sender)
      if (newState.stack == 0) {
        val endTime = System.currentTimeMillis
        sender ! Result(newState.goal, endTime - startTime)
        states -= id
      } else {
        states += (id -> newState)
      }
      ()
    }
  }

}

object Aggregater {

  case class Start(field: Field, initSearchDepth: Int, search: ActorRef)
  case class Push(id: Long, stack: Long, goal: Long)
  case class Result(count: Long, time: Long)

  case class State(stack: Long, goal: Long, startTime: Long, sender: ActorRef)

  def startAggregaterNode() = {
    import com.typesafe.config.ConfigFactory

    val actorName = ConfigFactory.load.getString("takashi.prob1.akka.aggregater.name")
    val system = {
      val name = ConfigFactory.load.getString("takashi.prob1.akka.name")
      ActorSystem(name)
    }
    system.actorOf(Props[Aggregater], actorName)
    ()
  }
}
