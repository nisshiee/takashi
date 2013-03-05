package org.nisshiee.takashi.prob2

case class Field(chestnutMap: Map[Point, Int], width: Int, height: Int)

object Field {

  implicit class RichField(val underlying: Field) extends AnyVal {

    def checkInside: Point => Boolean = {
      case Point(x, _) if x < 0 => false
      case Point(x, _) if x >= underlying.width => false
      case Point(_, y) if y < 0 => false
      case Point(_, y) if y >= underlying.height => false
      case _ => true
    }

    def chestnuts: Point => Int = {
      case p if checkInside(p) => underlying.chestnutMap.get(p) getOrElse 0
      case _ => 0
    }
  }

  def read(file: String): Field = {
    import com.github.tototoshi.csv.CSVReader
    import scalaz._, Scalaz._

    val csv = CSVReader.open(file).all()

    val chestnutMap = csv.zipWithIndex flatMap {
      case (l, y) => l.zipWithIndex map {
        case (str, x) => Point(x, y) -> (str.parseInt | 0)
      }
    } toMap

    val width = (chestnutMap map { case (p, _) => p.x } max) + 1
    val height = (chestnutMap map { case (p, _) => p.y } max) + 1

    Field(chestnutMap, width, height)
  }
}
