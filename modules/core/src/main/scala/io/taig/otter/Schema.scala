package io.taig.otter

sealed trait Schema[+A, B] extends Product, Serializable

sealed trait Collection[+A, B] extends Schema[A, B]

object Collection:
  final case class Root[F[_], A](schema: F[A]) extends Collection[F[A], Vector[A]]

final case class Primitive[A](tpe: Type[A]) extends Schema[Nothing, A]

sealed trait Tuple[+A, B] extends Schema[A, B]

object Tuple:
  case object Empty extends Tuple[Nothing, Unit]

  final case class One[F[_], A](schema: F[A]) extends Tuple[F[A], A]

  final case class Product[F[_], A, G[_], B](left: F[A], right: G[B]) extends Tuple[F[A] | G[B], (A, B)]
