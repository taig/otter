package io.taig.otter

import cats.data.Chain
import io.taig.otter.validation.Validation

sealed trait Tuple[+A, B] extends Tuple.Reader[A, B], Tuple.Writer[A, B]:
  final def optional: Tuple[A, Option[B]] = Tuple.Optional(this)

  final override def product[S[_, _]: TupleInvariant, C, D](schema: S[C, D]): Tuple[A | C, (B, D)] =
    Tuple.Product[S, A, B, C, D](this, schema)

object Tuple:
  sealed trait Reader[+A, +B]:
    def schemas: Chain[A]

    def optional: Tuple.Reader[A, Option[B]]

    def product[S[_, _]: TupleInvariant, C, D](schema: S[C, D]): Tuple.Reader[A | C, (B, D)] = ???

  sealed trait Writer[+A, -B]:
    def schemas: Chain[A]

    def optional: Tuple.Writer[A, Option[B]]

    def product[S[_, _]: TupleInvariant, C, D](schema: S[C, D]): Tuple.Writer[A | C, (B, D)] = ???

  case object Empty extends Tuple[Nothing, Unit]:
    override def schemas: Chain[Nothing] = Chain.empty

  final case class One[S[_, _], A, B](schema: S[A, B], unwrap: S[A, B] => Schema[A, B]) extends Tuple[S[A, B], B]:
    override def schemas: Chain[S[A, B]] = Chain.one(schema)

  final case class Optional[A, B](tuple: Tuple[A, B]) extends Tuple[A, Option[B]]:
    export tuple.schemas

  final case class Product[S[_, _]: TupleInvariant, A, B, C, D](left: Tuple[A, B], right: S[C, D])
      extends Tuple[A | C, (B, D)]:
    override def schemas: Chain[A | C] = left.schemas ++ right.schemas

  given TupleInvariant[Tuple] = new TupleInvariant[Tuple]:
    extension [A, B](self: Tuple[A, B])
      override def optional: Tuple[A, Option[B]] = self.optional
      override def product[C, D](tuple: Tuple[C, D]): Tuple[A | C, (B, D)] = self.product(tuple)
      override def schemas: Chain[A] = self.schemas
      override def toTuple: Tuple[Tuple[A, B], B] = Tuple.One(self, identity)

trait TupleInvariant[F[_, _]] extends SchemaInvariant[F, F]:
  extension [A, B](self: F[A, B])
    def schemas: Chain[A]
    def product[C, D](tuple: F[C, D]): F[A | C, (B, D)]
    def optional: F[A, Option[B]]
