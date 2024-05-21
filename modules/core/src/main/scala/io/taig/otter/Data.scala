package io.taig.otter

sealed trait Data[+F[_], +A, B] extends Product, Serializable

sealed trait Collection[+F[_], +A, B] extends Data[F, A, B]

object Collection:
  final case class Root[F[_], A](schema: F[A]) extends Collection[F, F[A], Vector[A]]

sealed trait Primitive[A] extends Data[Nothing, Nothing, A]

object Primitive:
  final case class Root[A](tpe: Type[A]) extends Primitive[A]

sealed trait Tuple[+F[_], +A, B] extends Data[F, A, B]

object Tuple:
  final case class Product[F[_], A, B, C, D](left: Tuple[F, A, B], right: Tuple[F, C, D])
      extends Tuple[F, A | C, (B, D)]

  final case class Root[F[_], A](schema: F[A]) extends Tuple[F, F[A], A]
