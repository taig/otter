package io.taig.otter

import cats.data.Chain

sealed trait Data[+F[+_], B] extends Product, Serializable

sealed trait Collection[+F[+_], B] extends Data[F, B]

object Collection:
  final case class Root[F[+_], B](schema: F[Schema[F, B]]) extends Collection[F, Vector[B]]

sealed trait Primitive[A] extends Data[Nothing, A]

object Primitive:
  final case class Root[A](tpe: Type[A]) extends Primitive[A]

sealed trait Tuple[+F[+_], B] extends Data[F, B]

object Tuple:
  final case class Product[+F[+_], B, D](left: Tuple[F, B], right: Tuple[F, D]) extends Tuple[F, (B, D)]

  final case class Root[F[+_], B](schema: F[Schema[F, B]]) extends Tuple[F, B]
