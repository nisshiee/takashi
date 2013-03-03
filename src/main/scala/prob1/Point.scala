package org.nisshiee.takashi.prob1

case class Point(x: Int, y: Int)

object Point {

  implicit def tuple2point: ((Int, Int)) => Point = {
    case (x, y) => Point(x, y)
  }
}
