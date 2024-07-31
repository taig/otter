package io.taig.otter

import scala.collection.immutable.Iterable
import cats.Eq
import cats.syntax.all.*

extension [A](self: Vector[A])
  def findWithRemainders[B](pf: PartialFunction[A, B]): (Option[B], Vector[A]) =
    val remainders = Vector.newBuilder[A]
    var result: Option[B] = None

    self.iterator.foreach: a =>
      if (result.isDefined || !pf.isDefinedAt(a)) then remainders += a
      else result = pf.lift(a)

    (result, remainders.result())

extension [A: Eq, B](self: Vector[(A, B)])
  def filterKeys(keys: Iterable[A]): (Vector[(A, B)], Vector[(A, B)]) =
    val remainingKeys = keys.toBuffer
    val result = Vector.newBuilder[(A, B)]
    val remainders = Vector.newBuilder[(A, B)]

    self.foreach { case value @ (key, _) =>
      if remainingKeys.exists(_ === key)
      then
        remainingKeys -= key
        result += value
      else remainders += value
    }

    (result.result(), remainders.result())

extension [F[a] <: Iterable[a], A](self: F[A])
  def uncons: Option[(A, F[A])] = self.headOption.map((_, self.tail.asInstanceOf[F[A]]))
