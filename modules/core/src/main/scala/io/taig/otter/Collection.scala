package io.taig.otter

import cats.data.Chain
import io.taig.otter.validation.Constraint

sealed trait Collection[+M, +A, B] extends Collection.Reader[M, A, B], Collection.Writer[M, A, B]:
  final override def optional: Collection[M, A, Option[B]] = Collection.Optional(this)
  final def ivalidate[V1, V2, C](validation: SchemaValidation[B, V1, V2, C])(f: C => B): Collection[M, A, C] =
    Collection.Modify(this, validation, f)

object Collection:
  sealed trait Reader[+M, +A, +B] extends Product, Serializable:
    def constraints: Chain[Constraint[?]]
    def optional: Collection.Reader[M, A, Option[B]] = Reader.Optional(this)
    final def validate[V1, V2, C](validation: SchemaValidation[B, V1, V2, C]): Collection.Reader[M, A, C] =
      Reader.Modify(this, validation)

  object Reader:
    final case class Modify[M, A, B, V1, V2, C](
        self: Collection.Reader[M, A, B],
        validation: SchemaValidation[B, V1, V2, C]
    ) extends Collection.Reader[M, A, C]:
      override def constraints: Chain[Constraint[?]] = validation.constraints

    final case class Optional[M, A, B](self: Collection.Reader[M, A, B]) extends Collection.Reader[M, A, Option[B]]:
      export self.constraints

    final case class Root[M, +A <: Schema.Reader[M, ?, B], B](schema: A) extends Collection.Reader[M, A, Vector[B]]:
      override def constraints: Chain[Constraint[?]] = Chain.empty

  sealed trait Writer[+M, +A, -B] extends Product, Serializable:
    final def contramap[C](f: C => B): Collection.Writer[M, A, C] = Writer.Modify(this, f)
    def optional: Collection.Writer[M, A, Option[B]] = Writer.Optional(this)

  object Writer:
    final case class Modify[M, A, B, C](self: Collection.Writer[M, A, B], f: C => B) extends Collection.Writer[M, A, C]

    final case class Optional[M, A, B](self: Collection.Writer[M, A, B]) extends Collection.Writer[M, A, Option[B]]

    final case class Root[M, +A <: Schema.Writer[M, ?, B], B](schema: A) extends Collection.Writer[M, A, Vector[B]]

  final case class Modify[M, A, B, V1, V2, C](
      self: Collection[M, A, B],
      validation: SchemaValidation[B, V1, V2, C],
      f: C => B
  ) extends Collection[M, A, C]:
    override def constraints: Chain[Constraint[?]] = self.constraints ++ validation.constraints

  final case class Optional[M, A, B](self: Collection[M, A, B]) extends Collection[M, A, Option[B]]:
    export self.constraints

  final case class Root[M, +A <: Schema[M, ?, B], B](schema: A) extends Collection[M, A, Vector[B]]:
    override def constraints: Chain[Constraint[?]] = Chain.empty
