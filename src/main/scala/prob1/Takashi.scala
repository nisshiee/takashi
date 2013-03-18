package org.nisshiee.takashi.prob1

import scalaz._, Scalaz._, Free._

object Takashi {

  implicit class RichPoint(val underlying: Point) extends AnyVal {

    def +(p: Point) = Point(underlying.x + p.x, underlying.y + p.y)
  }

  sealed trait FieldStatus
  case object Goal extends FieldStatus
  case object Enabled extends FieldStatus
  case object NotEnabled extends FieldStatus

  implicit class RichField(val underlying: Field) extends AnyVal {

    def status(p: Point): FieldStatus = 
      if (p == underlying.goal) Goal
      else if (underlying.enableds contains p) Enabled
      else NotEnabled
  }

  def routeCount(f: Field): Int = routeCountRec(f).run

  def routeCountRec: Field => Trampoline[Int] = { f =>

    def next(f: Field, step: Point): Trampoline[Int] = {
      val p = f.start + step
      f status p match {
        case Goal => Return(1)
        case NotEnabled => Return(0)
        case Enabled =>
          Suspend { () => routeCountRec(f.copy(f.enableds - f.start, p)) }
      }
    }

    f match {
      case Field(_, s, g) if s == g => Return(1)
      case f => for {
        up <- next(f, (0, -1))
        down <- next(f, (0, 1))
        left <- next(f, (-1, 0))
        right <- next(f, (1, 0))
      } yield up + down + left + right
    }
  }

}
