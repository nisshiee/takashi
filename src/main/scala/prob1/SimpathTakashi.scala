package org.nisshiee.takashi.prob1

import scalaz._, Scalaz._

object SimpathTakashi {

  import Takashi._

  def routeCount(f: Field): Long = {
    def simpath(p: List[EdgeNode], es: Stream[(Point, Point)], lastFocus: Point): Long = es match {
      case Stream.Empty => 0L
      case e #:: t => {
        val checked = (lastFocus, e._1, f.start) match {
          case (l, c, _) if l == c => p
          case (l, _, s) if l == s => p filter {
            case EdgeNode(ma, _) => ma.get(s) match {
              case Some(EndOfPath(_)) => true
              case _ => false
            }
          }
          case (l, _, _) => p filter {
            case EdgeNode(ma, _) => ma.get(l) match {
              case Some(EndOfPath(_)) => false
              case _ => true
            }
          }
        }
        val nextNodes = checked flatMap { n => List(n.hi(e, f.start, f.goal), n.lo) }
        val nextEdges = {
          val beforeAggregate: List[EdgeNode] = nextNodes collect { case n: EdgeNode => n }
          def aggregate: List[EdgeNode] => List[EdgeNode] = {
            case Nil => Nil
            case h :: t => {
              val (same, others) = t partition (h.ma =:= _.ma)
              val count = h.count + same.map(_.count).sum
              val aggregated = h.copy(count = count)
              aggregated :: aggregate(others)
            }
          }
          aggregate(beforeAggregate)
        }
        val goal = nextNodes collect { case Arrive(c) => c } sum

        goal + simpath(nextEdges, t, e._1)
      }
    }

    simpath(EdgeNode(initialMates(f), 1L) :: Nil, edgeList(f), f.start)
  }

  type MateArray = Map[Point, Mate]

  implicit class RichMateArray (val ma: MateArray) extends AnyVal {

    def =:=(o: MateArray) = {
      def f: Mate => Option[Point] = {
        case EndOfPath(p) => p.some
        case _ => none
      }
      val ma1 = ma mapValues f
      val ma2 = o mapValues f
      ma1 == ma2
    }

    def hi: ((Point, Point)) => Option[MateArray] = { 
      case (p1, p2) => (ma.get(p1), ma.get(p2)) match {
        case (None, _) => none
        case (_, None) => none
        case (Some(m1), Some(m2)) => (m1, m2) match {
          case (OnPath, _) => none
          case (_, OnPath) => none
          case (OutOfPath, OutOfPath) => (ma + (p1 -> EndOfPath(p2)) + (p2 -> EndOfPath(p1))).some
          case (OutOfPath, EndOfPath(p)) =>
            (ma + (p1 -> EndOfPath(p)) + (p2 -> OnPath) + (p -> EndOfPath(p1))).some
          case (EndOfPath(p), OutOfPath) =>
            (ma + (p2 -> EndOfPath(p)) + (p1 -> OnPath) + (p -> EndOfPath(p2))).some
          case (EndOfPath(op1), EndOfPath(op2)) =>
            (ma + (p1 -> OnPath) + (p2 -> OnPath) +
              (op1 -> EndOfPath(op2)) + (op2 -> EndOfPath(op1))).some
        }
      }
    }
  }


  def initialMates(f: Field): MateArray =
    (f.enableds + f.start + f.goal) map (_ -> OutOfPath) toMap

  def edgeList(f: Field): Stream[(Point, Point)] = {
    def fromPoint(reached: Set[Point])(p: Point) =
      Stream(Point(-1, 0), Point(0, -1), Point(1, 0), Point(0, 1))
        .map(p + _)
        .filterNot(reached.apply)
        .filter(f.enableds.apply)
        .map(p -> _)

    def findrec: (Stream[Point], Set[Point]) => Stream[(Point, Point)] = {
      case (Stream.Empty, _) => Stream.Empty
      case (p #:: t, reached) => {
        val edges = fromPoint(reached)(p)
        val nextPs = edges map (_._2) filterNot t.contains

        edges ++ findrec(t ++ nextPs, reached + p)
      }
    }

    findrec(Stream(f.start), Set())
  }

  sealed trait Mate
  case object OutOfPath extends Mate
  case object OnPath extends Mate
  case class EndOfPath(other: Point) extends Mate

  sealed trait Node
  case class EdgeNode(ma: MateArray, count: Long) extends Node
  case object Miss extends Node
  case class Arrive(count: Long) extends Node

  def checkArrive(ma: MateArray, start: Point, goal: Point) = ma forall {
    case (p1, EndOfPath(p2)) if p1 == start && p2 == goal => true
    case (p1, EndOfPath(p2)) if p2 == start && p1 == goal => true
    case (_, OutOfPath) => true
    case (_, OnPath) => true
    case _ => false
  }

  implicit class RichEdgeNode(val n: EdgeNode) extends AnyVal {

    def hi(edge: (Point, Point), start: Point, goal: Point): Node = n.ma hi edge match {
      case Some(ma) if checkArrive(ma, start, goal) => Arrive(n.count)
      case Some(ma) => EdgeNode(ma, n.count)
      case None => Miss
    }

    def lo = n
  }
}
