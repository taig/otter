package io.taig.otter

sealed trait Schema[+F[_], +D[_[_], _], +A <: Wrapper[F, D, ?, ?, ?], B] extends Product, Serializable

sealed trait Primitive[A] extends Schema[Nothing, Nothing, Nothing, A]

object Primitive:
  final case class Root[A](tpe: Type[A]) extends Primitive[A]

sealed trait Collection[+F[_], +D[_[_], _], +A <: Wrapper[F, D, ?, ?, ?], B] extends Schema[F, D, A, B]

object Collection:
  final case class Root[+F[_], +D[_[_], _], A <: Wrapper[F, D, ?, ?, B], B](schema: A)
      extends Collection[F, D, A, Vector[B]]
