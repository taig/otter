package io.taig.otter

sealed trait Data[+F[_], A] extends Product, Serializable

sealed trait Collection[+F[_], A] extends Data[F, A]

object Collection:
  final case class Root[F[_], A](schema: F[A]) extends Collection[F, Vector[A]]

sealed trait Primitive[A] extends Data[Nothing, A]

object Primitive:
  final case class Root[A](tpe: Type[A]) extends Primitive[A]

sealed trait Tuple[+F[_], A] extends Data[F, A]

object Tuple:
  final case class Product[F[_], A, B](left: Tuple[F, A], right: Tuple[F, B]) extends Tuple[F, (A, B)]

  final case class Root[F[_], A](schema: F[A]) extends Tuple[F, A]
