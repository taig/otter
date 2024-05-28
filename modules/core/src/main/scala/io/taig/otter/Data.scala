package io.taig.otter

private type Parent[F[_], D[_[_], _], A] = F[D[Schema[Data[F, D, ?, *], *], A]]

sealed trait Data[+F[_], +D[_[_], _], +A, B] extends Product, Serializable

sealed trait Primitive[A] extends Data[Nothing, Nothing, Nothing, A]

object Primitive:
  final case class Root[A](tpe: Type[A]) extends Primitive[A]

sealed trait Collection[+F[_], D[_[_], _], +A, B] extends Data[F, D, A, B]

object Collection:
  final case class Root[+F[_], D[_[_], _], +A <: Parent[F, D, B], B](schema: A) extends Collection[F, D, A, Vector[B]]
