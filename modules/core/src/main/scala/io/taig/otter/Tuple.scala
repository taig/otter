package io.taig.otter

import cats.data.Chain

sealed trait Tuple[+S[+_], +A, B] extends Tuple.Reader[S, A, B], Tuple.Writer[S, A, B]:
  def schemas: Chain[A]

  final def product[T[+a] >: S[a], C, D](tuple: Tuple[T, C, D]): Tuple[T, A | C, (B, D)] =
    ??? // Tuple.Product(this, tuple)

object Tuple:
  sealed trait Reader[+S[+_], +A, +B]

  sealed trait Writer[+S[+_], +A, -B]

  case object Empty extends Tuple[Nothing, Nothing, Unit]:
    override def schemas: Chain[Nothing] = Chain.empty

  final case class One[S[+_], T[a] <: Schema[S, ?, a], A](schema: S[T[A]]) extends Tuple[S, S[T[A]], A]:
    override def schemas: Chain[S[T[A]]] = Chain.one(schema)

  final case class Product[S[+_], A, B, C, D](left: Tuple[S, A, B], right: Tuple[S, C, D])
      extends Tuple[S, A | C, (B, D)]:
    override def schemas: Chain[A | C] = left.schemas ++ right.schemas
