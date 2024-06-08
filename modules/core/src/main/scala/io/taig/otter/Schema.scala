package io.taig.otter

import cats.syntax.all.*
import cats.data.Chain
import io.taig.otter.validation.Constraint
import io.taig.otter.Schema.Reader

sealed trait Schema[+F[+_], +A, B] extends Schema.Reader[F, A, B], Schema.Writer[F, A, B]:
  def ivalidate[V1, V2, C](validation: SchemaValidation[B, V1, V2, C])(f: C => B): Schema[F, A, C]
  override def optional: Schema[F, A, Option[B]]

object Schema:
  sealed trait Reader[+F[+_], +A, +B] extends Product, Serializable:
    def constraints: Chain[Constraint[?]]
    def optional: Schema.Reader[F, A, Option[B]]
    def validate[V1, V2, C](validation: SchemaValidation[B, V1, V2, C]): Schema.Reader[F, A, C]

  sealed trait Writer[+F[+_], +A, -B] extends Product, Serializable:
    def contramap[C](f: C => B): Schema.Writer[F, A, C]
    def optional: Schema.Writer[F, A, Option[B]]

sealed trait Collection[+F[+_], +A, B] extends Schema[F, A, B], Collection.Reader[F, A, B], Collection.Writer[F, A, B]:
  final def ivalidate[V1, V2, C](validation: SchemaValidation[B, V1, V2, C])(f: C => B): Collection[F, A, C] =
    Collection.Modify(this, validation, f)
  final override def optional: Collection[F, A, Option[B]] = Collection.Optional(this)
  override def schema: F[Schema[F, ?, ?]]

object Collection:
  sealed trait Reader[+F[+_], +A, +B] extends Schema.Reader[F, A, B]:
    def constraints: Chain[Constraint[?]]
    def optional: Collection.Reader[F, A, Option[B]] = Reader.Optional(this)
    def schema: F[Schema.Reader[F, ?, ?]]
    final def validate[V1, V2, C](validation: SchemaValidation[B, V1, V2, C]): Collection.Reader[F, A, C] =
      Reader.Modify(this, validation)

  object Reader:
    final case class Modify[F[+_], A, B, V1, V2, C](
        self: Collection.Reader[F, A, B],
        validation: SchemaValidation[B, V1, V2, C]
    ) extends Collection.Reader[F, A, C]:
      export self.schema
      override def constraints: Chain[Constraint[?]] = validation.constraints

    final case class Optional[F[+_], A, B](self: Collection.Reader[F, A, B]) extends Collection.Reader[F, A, Option[B]]:
      export self.{constraints, schema}

    final case class Root[F[+_], +A <: F[Schema.Reader[F, ?, B]], B](schema: A)
        extends Collection.Reader[F, A, Vector[B]]:
      override def constraints: Chain[Constraint[?]] = Chain.empty

  sealed trait Writer[+F[+_], +A, -B] extends Schema.Writer[F, A, B]:
    final def contramap[C](f: C => B): Collection.Writer[F, A, C] = Writer.Modify(this, f)
    def optional: Collection.Writer[F, A, Option[B]] = Writer.Optional(this)
    def schema: F[Schema.Writer[F, ?, ?]]

  object Writer:
    final case class Modify[F[+_], A, B, C](self: Collection.Writer[F, A, B], f: C => B)
        extends Collection.Writer[F, A, C]:
      export self.schema

    final case class Optional[F[+_], A, B](self: Collection.Writer[F, A, B]) extends Collection.Writer[F, A, Option[B]]:
      export self.schema

    final case class Root[F[+_], +A <: F[Schema.Writer[F, ?, B]], B](schema: A)
        extends Collection.Writer[F, A, Vector[B]]

  final case class Modify[F[+_], A, B, V1, V2, C](
      self: Collection[F, A, B],
      validation: SchemaValidation[B, V1, V2, C],
      f: C => B
  ) extends Collection[F, A, C]:
    export self.schema
    override def constraints: Chain[Constraint[?]] = self.constraints ++ validation.constraints

  final case class Optional[F[+_], A, B](self: Collection[F, A, B]) extends Collection[F, A, Option[B]]:
    export self.{constraints, schema}

  final case class Root[F[+_], +A <: F[Schema[F, ?, B]], B](schema: A) extends Collection[F, A, Vector[B]]:
    override def constraints: Chain[Constraint[?]] = Chain.empty

sealed trait Primitive[A] extends Schema[Nothing, Nothing, A], Primitive.Reader[A], Primitive.Writer[A]:
  final override def optional: Primitive[Option[A]] = Primitive.Optional(this)
  def ivalidate[V1, V2, B](validation: SchemaValidation[A, V1, V2, B])(f: B => A): Primitive[B] =
    Primitive.Modify(this, validation, f)

object Primitive:
  sealed trait Required[A] extends Primitive[A], Primitive.Required.Reader[A], Primitive.Required.Writer[A]:
    final override def ivalidate[V1, V2, B](validation: SchemaValidation[A, V1, V2, B])(
        f: B => A
    ): Primitive.Required[B] = Required.Modify(this, validation, f)

  object Required:
    sealed trait Reader[+A] extends Primitive.Reader[A]:
      override def validate[V1, V2, C](
          validation: SchemaValidation[A, V1, V2, C]
      ): Primitive.Required.Reader[C] = Reader.Modify(this, validation)

    object Reader:
      final case class Modify[A, V1, V2, B](
          self: Primitive.Required.Reader[A],
          validation: SchemaValidation[A, V1, V2, B]
      ) extends Primitive.Required.Reader[B]:
        export self.tpe
        override def constraints: Chain[Constraint[?]] = validation.constraints

    sealed trait Writer[-A] extends Primitive.Writer[A]:
      final override def contramap[C](f: C => A): Primitive.Required.Writer[C] = Writer.Modify(this, f)

    object Writer:
      final case class Modify[A, B](self: Primitive.Required.Writer[A], f: B => A) extends Primitive.Required.Writer[B]:
        export self.tpe

    final case class Modify[A, V1, V2, B](
        self: Primitive.Required[A],
        validation: SchemaValidation[A, V1, V2, B],
        f: B => A
    ) extends Primitive.Required[B]:
      export self.tpe
      override def constraints: Chain[Constraint[?]] = self.constraints ++ validation.constraints

    final case class Root[A](tpe: Type[A]) extends Primitive.Required[A]:
      override def constraints: Chain[Constraint[?]] = Chain.empty

  sealed trait Reader[+A] extends Schema.Reader[Nothing, Nothing, A]:
    def constraints: Chain[Constraint[?]]
    def optional: Primitive.Reader[Option[A]] = Reader.Optional(this)
    def tpe: Type[?]
    def validate[V1, V2, C](validation: SchemaValidation[A, V1, V2, C]): Primitive.Reader[C] =
      Reader.Modify(this, validation)

  object Reader:
    final case class Modify[A, V1, V2, B](self: Primitive.Reader[A], validation: SchemaValidation[A, V1, V2, B])
        extends Primitive.Reader[B]:
      export self.tpe
      override def constraints: Chain[Constraint[?]] = validation.constraints

    final case class Optional[A](self: Primitive.Reader[A]) extends Primitive.Reader[Option[A]]:
      export self.{constraints, tpe}

  sealed trait Writer[-A] extends Schema.Writer[Nothing, Nothing, A]:
    def contramap[C](f: C => A): Primitive.Writer[C] = Writer.Modify(this, f)
    def optional: Primitive.Writer[Option[A]] = Writer.Optional(this)
    def tpe: Type[?]

  object Writer:
    final case class Modify[A, B](self: Primitive.Writer[A], f: B => A) extends Primitive.Writer[B]:
      export self.tpe

    final case class Optional[A](self: Primitive.Writer[A]) extends Primitive.Writer[Option[A]]:
      export self.tpe

  final case class Modify[A, V1, V2, B](self: Primitive[A], validation: SchemaValidation[A, V1, V2, B], f: B => A)
      extends Primitive[B]:
    export self.tpe
    override def constraints: Chain[Constraint[?]] = self.constraints ++ validation.constraints

  final case class Optional[A](self: Primitive[A]) extends Primitive[Option[A]]:
    export self.{constraints, tpe}

sealed trait Tuple[+F[+_], +A, B] extends Schema[F, A, B], Tuple.Reader[F, A, B], Tuple.Writer[F, A, B]:
  final def ivalidate[V1, V2, C](validation: SchemaValidation[B, V1, V2, C])(f: C => B): Tuple[F, A, C] =
    Tuple.Modify(this, validation, f)
  final override def optional: Tuple[F, A, Option[B]] = Tuple.Optional(this)
  override def schemas: Chain[F[Schema[F, ?, ?]]]

object Tuple:
  sealed trait Reader[+F[+_], +A, +B] extends Schema.Reader[F, A, B]:
    def schemas: Chain[F[Schema.Reader[F, ?, ?]]]
    def constraints: Chain[Constraint[?]]
    def optional: Tuple.Reader[F, A, Option[B]] = Reader.Optional(this)
    final def validate[C, D, E](validation: SchemaValidation[B, C, D, E]): Tuple.Reader[F, A, E] =
      Reader.Modify(this, validation)

  object Reader:
    case object Empty extends Tuple.Reader[Nothing, Nothing, Unit]:
      override def constraints: Chain[Constraint[?]] = Chain.empty
      override def schemas: Chain[Nothing] = Chain.empty

    final case class Modify[F[+_], A, B, V1, V2, C](
        self: Tuple.Reader[F, A, B],
        validation: SchemaValidation[B, V1, V2, C]
    ) extends Tuple.Reader[F, A, C]:
      export self.schemas
      override def constraints: Chain[Constraint[?]] = self.constraints ++ validation.constraints

    final case class One[F[+_], +A <: F[Schema.Reader[F, ?, B]], B](schema: A) extends Tuple.Reader[F, A, B]:
      override def constraints: Chain[Constraint[?]] = Chain.empty
      override def schemas: Chain[A] = Chain.one(schema)

    final case class Optional[F[+_], A, B](self: Tuple.Reader[F, A, B]) extends Tuple.Reader[F, A, Option[B]]:
      export self.{constraints, schemas}

    final case class Zip[F[+_], A, B, +C <: F[Schema.Reader[F, ?, D]], D](
        left: Tuple.Reader[F, A, B],
        right: C
    ) extends Tuple.Reader[F, A | C, (B, D)]:
      override def constraints: Chain[Constraint[?]] = left.constraints
      override def schemas: Chain[F[Schema.Reader[F, ?, ?]]] = left.schemas :+ right

  sealed trait Writer[+F[+_], +A, -B] extends Schema.Writer[F, A, B]:
    final def contramap[C](f: C => B): Tuple.Writer[F, A, C] = Writer.Modify(this, f)
    def optional: Tuple.Writer[F, A, Option[B]] = Writer.Optional(this)
    def schemas: Chain[F[Schema.Writer[F, ?, ?]]]

  object Writer:
    case object Empty extends Tuple.Writer[Nothing, Nothing, Unit]:
      override def schemas: Chain[Nothing] = Chain.empty

    final case class Modify[F[+_], A, B, C](self: Tuple.Writer[F, A, B], f: C => B) extends Tuple.Writer[F, A, C]:
      export self.schemas

    final case class One[F[+_], +A <: F[Schema.Writer[F, ?, B]], B](schema: A) extends Tuple.Writer[F, A, B]:
      override def schemas: Chain[A] = Chain.one(schema)

    final case class Optional[F[+_], A, B](self: Tuple.Writer[F, A, B]) extends Tuple.Writer[F, A, Option[B]]:
      export self.schemas

    final case class Zip[F[+_], A, B, +C <: F[Schema.Writer[F, ?, D]], D](
        left: Tuple.Writer[F, A, B],
        right: C
    ) extends Tuple.Writer[F, A | C, (B, D)]:
      override def schemas: Chain[F[Schema.Writer[F, ?, ?]]] = left.schemas :+ right

  case object Empty extends Tuple[Nothing, Nothing, Unit]:
    override def constraints: Chain[Constraint[?]] = Chain.empty
    override def schemas: Chain[Nothing] = Chain.empty

  final case class Modify[F[+_], A, B, V1, V2, C](
      self: Tuple[F, A, B],
      validation: SchemaValidation[B, V1, V2, C],
      f: C => B
  ) extends Tuple[F, A, C]:
    export self.schemas
    override def constraints: Chain[Constraint[?]] = self.constraints ++ validation.constraints

  final case class One[F[+_], +A <: F[Schema[F, ?, B]], B](schema: A) extends Tuple[F, A, B]:
    override def constraints: Chain[Constraint[?]] = Chain.empty
    override def schemas: Chain[A] = Chain.one(schema)

  final case class Optional[F[+_], A, B](self: Tuple[F, A, B]) extends Tuple[F, A, Option[B]]:
    export self.{constraints, schemas}

  final case class Zip[F[+_], A, B, +C <: F[Schema[F, ?, D]], D](left: Tuple[F, A, B], right: C)
      extends Tuple[F, A | C, (B, D)]:
    override def constraints: Chain[Constraint[?]] = left.constraints
    override def schemas: Chain[F[Schema[F, ?, ?]]] = left.schemas :+ right
