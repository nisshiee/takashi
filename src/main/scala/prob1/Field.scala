package org.nisshiee.takashi.prob1

import scalaz._, Scalaz._

case class Field(enableds: Set[Point], start: Point, goal: Point)

object Field {

  import com.github.tototoshi.csv.CSVReader

  def read(file: String): Field = {
    val csv = CSVReader.open(file).all()

    val set = csv.zipWithIndex flatMap {
      case (l, y) => l.zipWithIndex flatMap {
        case ("0", x) => List(Point(x, y))
        case _ => Nil
      }
    } toSet

    val goal: Point = csv.lift(csv.size - 1) flatMap
      { l => (l.size > 0) option Point(l.size - 1, csv.size - 1) } getOrElse
      Point(0, 0)

    Field(set, Point(0, 0), goal)
  }
}
