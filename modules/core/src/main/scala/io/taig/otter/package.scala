package io.taig.otter

import scala.collection.immutable.Iterable

extension [A](self: Vector[A])
  def findWithRemainders[B](pf: PartialFunction[A, B]): (Option[B], Vector[A]) =
    val remainders = Vector.newBuilder[A]
    var result: Option[B] = None

    self.iterator.foreach: a =>
      if (result.isDefined || !pf.isDefinedAt(a)) then remainders += a
      else result = pf.lift(a)

    (result, remainders.result())

extension [A](self: Vector[(String, A)])
  def filterKeys(keys: Iterable[String]): Vector[(String, A)] =
    val result = Vector.newBuilder[(String, A)]

    keys.foreach: reference =>
      self.find { case (key, _) => reference == key }.foreach(result += _)

    result.result()

extension [F[a] <: Iterable[a], A](self: F[A])
  def uncons: Option[(A, F[A])] = self.headOption.map((_, self.tail.asInstanceOf[F[A]]))
