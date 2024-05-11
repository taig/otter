package io.taig.otter

import cats.data.Chain
import io.taig.otter.validation.Validation

sealed trait Tuple[+A, B] extends Tuple.Reader[A, B], Tuple.Writer[A, B]:
  final def optional: Tuple[A, Option[B]] = Tuple.Optional(this)

  final override def product[S[_, _]: TupleArrow, C, D](schema: S[C, D]): Tuple[A | C, (B, D)] =
    Tuple.Product[S, A, B, C, D](this, schema)

object Tuple:
  sealed trait Reader[+A, +B]:
    def schemas: Chain[A]

    def optional: Tuple.Reader[A, Option[B]]

    def product[S[_, _]: TupleArrow, C, D](schema: S[C, D]): Tuple.Reader[A | C, (B, D)] = ???

  sealed trait Writer[+A, -B]:
    def schemas: Chain[A]

    def optional: Tuple.Writer[A, Option[B]]

    def product[S[_, _]: TupleArrow, C, D](schema: S[C, D]): Tuple.Writer[A | C, (B, D)] = ???

  case object Empty extends Tuple[Nothing, Unit]:
    override def schemas: Chain[Nothing] = Chain.empty

  final case class One[S[_, _], A, B](schema: S[A, B]) extends Tuple[S[A, B], B]:
    override def schemas: Chain[S[A, B]] = Chain.one(schema)

  final case class Optional[A, B](tuple: Tuple[A, B]) extends Tuple[A, Option[B]]:
    export tuple.schemas

  final case class Product[S[_, _]: TupleArrow, A, B, C, D](left: Tuple[A, B], right: S[C, D])
      extends Tuple[A | C, (B, D)]:
    override def schemas: Chain[A | C] = left.schemas ++ right.schemas

  given [A]: TupleInvariant[Tuple[A, *]] with
    override def optional[B](fa: Tuple[A, B]): Tuple[A, Option[B]] = fa.optional
    override def ivalidate[B, C, D, E](fa: Tuple[A, B])(validation: Validation[B, C, D, E])(f: E => B): Tuple[A, E] =
      ???

  given TupleArrow[Tuple] = new TupleArrow[Tuple]:
    extension [A, B](self: Tuple[A, B])
      override def optional: Tuple[A, Option[B]] = self.optional
      override def product[C, D](tuple: Tuple[C, D]): Tuple[A | C, (B, D)] = self.product(tuple)
      override def schemas: Chain[A] = self.schemas

trait TupleInvariant[F[_]] extends SchemaInvariant[F, F]

trait TupleArrow[F[_, _]]:
  extension [A, B](self: F[A, B])
    def schemas: Chain[A]
    def product[C, D](tuple: F[C, D]): F[A | C, (B, D)]
    def optional: F[A, Option[B]]
