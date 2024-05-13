package io.taig.otter

import cats.data.Chain
import io.taig.otter.validation.Validation
import io.taig.otter.validation.Constraint

sealed trait Tuple[+S[+_], +A, B] extends Tuple.Reader[S, A, B], Tuple.Writer[S, A, B]:
  final def product[T[+a] >: S[a], C, D](tuple: Tuple[T, C, D]): Tuple[T, A | C, (B, D)] =
    Tuple.Product(this, tuple)

object Tuple:
  sealed trait Reader[+S[+_], +A, +B]:
    def constraints: Chain[Constraint[?]]

    def schemas: Chain[A]

    final def validate[C, D, E](validation: SchemaValidation[B, C, D, E]): Tuple.Reader[S, A, E] =
      Reader.Validate(this, validation)

  object Reader:
    final case class One[S[+_], T[a] <: Schema.Reader[S, ?, a], A](schema: S[T[A]]) extends Tuple.Reader[S, S[T[A]], A]:
      override def constraints: Chain[Constraint[?]] = Chain.empty
      override def schemas: Chain[S[T[A]]] = Chain.one(schema)

    final case class Validate[S[+_], A, B, C, D, E](
        self: Tuple.Reader[S, A, B],
        validation: SchemaValidation[B, C, D, E]
    ) extends Tuple.Reader[S, A, E]:
      export self.schemas
      override def constraints: Chain[Constraint[?]] = self.constraints ++ validation.constraints

  sealed trait Writer[+S[+_], +A, -B]:
    def schemas: Chain[A]

    final def contramap[C](f: C => B): Tuple.Writer[S, A, B] = ???

  case object Empty extends Tuple[Nothing, Nothing, Unit]:
    override def constraints: Chain[Constraint[?]] = Chain.empty
    override def schemas: Chain[Nothing] = Chain.empty

  final case class One[S[+_], T[a] <: Schema[S, ?, a], A](schema: S[T[A]]) extends Tuple[S, S[T[A]], A]:
    override def constraints: Chain[Constraint[?]] = Chain.empty
    override def schemas: Chain[S[T[A]]] = Chain.one(schema)

  final case class Optional[S[+_], A, B](self: Tuple[S, A, B]) extends Tuple[S, A, Option[B]]:
    export self.{constraints, schemas}

  final case class Product[S[+_], A, B, C, D](left: Tuple[S, A, B], right: Tuple[S, C, D])
      extends Tuple[S, A | C, (B, D)]:
    override def constraints: Chain[Constraint[?]] = left.constraints ++ right.constraints
    override def schemas: Chain[A | C] = left.schemas ++ right.schemas
