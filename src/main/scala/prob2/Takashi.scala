package org.nisshiee.takashi.prob2

import scala.annotation.tailrec

object Takashi {

  implicit class RichField(val underlying: Field) extends AnyVal {

    def start = Point(0, 0)
    def goal = Point(underlying.width - 1, underlying.height - 1)

    def skewline: Int => List[Point] = {
      case n if n < 0 => Nil
      case n if n > underlying.width + underlying.height - 2 => Nil
      case n => 0 to n map { x => Point(x, n - x) } filter underlying.checkInside toList
    }
  }

  implicit class RichPoint(val underlying: Point) extends AnyVal {

    def last = {
      val Point(x, y) = underlying
      List(Point(x - 1, y), Point(x, y - 1))
    }
  }

  def maxCount(f: Field): Int = maxCountRec(f, 1, List(f.start -> f.chestnuts(f.start)))

  @tailrec
  def maxCountRec(f: Field, skew: Int, last: List[(Point, Int)]): Int = {
    last collectFirst { case (p, n) if p == f.goal => n } match {
      case Some(n) => n
      case None => {
        val current = f skewline skew flatMap { p =>
          val currentNum = f chestnuts p
          val lastPositions = p.last
          last collect {
            case (p, n) if lastPositions contains p => n
          } match {
            case Nil => Nil
            case l => List(p -> (l.max + currentNum))
          }
        }
        maxCountRec(f, skew + 1, current)
      }
    }
  }
}
