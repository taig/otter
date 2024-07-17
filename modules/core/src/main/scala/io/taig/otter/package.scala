package io.taig.otter

import cats.data.Chain
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Iterable

extension [A](self: Chain[A])
  def findWithRemainders[B](pf: PartialFunction[A, B]): (Option[B], Chain[A]) =
    val remainders = ListBuffer.empty[A]
    var result: Option[B] = None

    self.iterator.foreach: a =>
      if (result.isDefined || !pf.isDefinedAt(a)) then remainders += a
      else result = pf.lift(a)

    (result, Chain.fromIterableOnce(remainders))

extension [F[a] <: Iterable[a], A](self: F[A])
  def uncons: Option[(A, F[A])] = self.headOption.map((_, self.tail.asInstanceOf[F[A]]))
