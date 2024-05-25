package io.taig.otter

sealed trait Data[+F[_], +D[_[_], _], B] extends Product, Serializable

sealed trait Primitive[A] extends Data[Nothing, Nothing, A]

object Primitive:
  final case class Root[A](tpe: Type[A]) extends Primitive[A]

sealed trait Collection[+F[_], +D[_[_], _], B] extends Data[F, D, B]

object Collection:
  final case class Root[F[_], D[_[_], _], A](schema: F[D[Schema[Data[F, D, *], *], A]])
      extends Collection[F, D, Vector[A]]
