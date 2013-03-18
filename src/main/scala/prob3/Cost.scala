package org.nisshiee.takashi.prob3

import scalaz._, Scalaz._

trait Cost[A] {

  def cost(a: A): Int
}

object Cost {

  def distanceCost[A](implicit d: Distance[A]): Cost[A @@ Tags.Distance] = new Cost[A @@ Tags.Distance] {
    override def cost(a: A @@ Tags.Distance) = d.distance(a)
  }

  implicit def distanceTaggedCost[A: Distance] = distanceCost[A]

  def feeCost[A](implicit f: Fee[A]): Cost[A @@ Tags.Fee] = new Cost[A @@ Tags.Fee] {
    override def cost(a: A @@ Tags.Fee) = f.fee(a)
  }

  implicit def feeTaggedCost[A: Fee] = feeCost[A]
}

trait CostOps[A] {
  def ins: Cost[A]
  def self: A

  def cost = ins cost self
}

trait ToCostOps {

  implicit def toCostOps[A](a: A)(implicit c: Cost[A]) = new CostOps[A] {
    override def ins = c
    override def self = a
  }
}


trait Distance[A] {

  def distance(a: A): Int
}

trait Fee[A] {

  def fee(a: A): Int
}

object Tags {
  sealed trait Distance
  def Distance[A](a: A): A @@ Distance = Tag[A, Distance](a)

  sealed trait Fee
  def Fee[A](a: A): A @@ Fee = Tag[A, Fee](a)
}
