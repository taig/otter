package io.taig.otter

sealed trait Schema[+A, B] extends Product, Serializable

sealed trait Primitive[A] extends Schema[Nothing, A]

object Primitive:
  final case class Root[A](tpe: Type[A]) extends Primitive[A]

sealed trait Collection[+A, B] extends Schema[A, B]

object Collection:
  final case class Root[F[_], A](schema: F[A]) extends Collection[F[A], Vector[A]]
