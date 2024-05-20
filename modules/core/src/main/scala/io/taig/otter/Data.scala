package io.taig.otter

import cats.data.Chain

sealed trait Data[+F[+_], A] extends Product, Serializable

sealed trait Collection[+F[+_], A] extends Data[F, A]

object Collection:
  final case class Root[F[+_], A](schema: F[Schema[F, Data, A]]) extends Collection[F, Vector[A]]

sealed trait Primitive[A] extends Data[Nothing, A]

object Primitive:
  final case class Root[A](tpe: Type[A]) extends Primitive[A]

sealed trait Tuple[+F[+_], A] extends Data[F, A]:
  def schemas: Chain[F[Schema[F, Data, A]]]

object Tuple:
  final case class Product[+F[+_], A, B](left: F[Schema[F, Data, A]], right: F[Schema[F, Data, B]])
      extends Tuple[F, (A, B)]:
    override def schemas: Chain[F[Schema[F, Data, (A, B)]]] = ???

  final case class Root[F[+_], A](schema: F[Schema[F, Data, A]]) extends Tuple[F, A]:
    override def schemas: Chain[F[Schema[F, Data, A]]] = Chain.one(schema)
