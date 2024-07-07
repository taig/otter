package io.taig.otter.http

import scala.collection.mutable.ListBuffer

extension [A](self: List[A])
  def findWithRemainders[B](pf: PartialFunction[A, B]): (Option[B], List[A]) =
    val remainders = ListBuffer.empty[A]
    var result: Option[B] = None

    self.iterator.foreach: a =>
      if (result.isDefined || !pf.isDefinedAt(a)) then remainders += a
      else result = pf.lift(a)

    (result, remainders.toList)
