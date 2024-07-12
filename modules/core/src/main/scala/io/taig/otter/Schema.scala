package io.taig.otter

import cats.syntax.all.*
import cats.data.Chain
import cats.data.NonEmptyChain
import io.taig.otter.validation.Validation
import io.taig.otter as Base
import scala.Product as SProduct
import io.taig.otter
import io.taig.enumeration.ext.Mapping
import io.taig.otter.Record.Reader

sealed trait Schema[-F, +O, A] extends Schema.Reader[F, O, A], Schema.Writer[F, O, A]:
  def imap[B](f: A => B)(g: B => A): Schema[F, O, B]
  override def optional: Schema[F, O, Option[A]]
  override def update(f: Metadata => Metadata): Schema[F, O, A]

object Schema:
  sealed trait Reader[-F, +B, +C] extends SProduct, Serializable:
    def metadata: Metadata
    def map[D](f: C => D): Schema.Reader[F, B, D]
    def optional: Schema.Reader[F, B, Option[C]]
    def update(f: Metadata => Metadata): Schema.Reader[F, B, C]

  sealed trait Writer[-F, +B, -C] extends SProduct, Serializable:
    def contramap[D](f: D => C): Schema.Writer[F, B, D]
    def metadata: Metadata
    def optional: Schema.Writer[F, B, Option[C]]
    def update(f: Metadata => Metadata): Schema.Writer[F, B, C]

sealed trait Value[-F, +B, C] extends Schema[F, B, C], Value.Reader[F, B, C], Value.Writer[F, B, C]:
  override def imap[D](f: C => D)(g: D => C): Value[F, B, D]
  override def optional: Value[F, B, Option[C]]
  override def update(f: Metadata => Metadata): Value[F, B, C]

object Value:
  sealed trait Required[-F, +B, C]
      extends Value[F, B, C],
        Value.Required.Reader[F, B, C],
        Value.Required.Writer[F, B, C]:
    override def imap[D](f: C => D)(g: D => C): Value.Required[F, B, D]
    override def update(f: Metadata => Metadata): Value.Required[F, B, C]

  object Required:
    sealed trait Reader[-F, +B, +C] extends Value.Reader[F, B, C]:
      override def map[D](f: C => D): Value.Required.Reader[F, B, D]
      override def update(f: Metadata => Metadata): Value.Required.Reader[F, B, C]

    object Reader:
      type Via[F, A] = Value.Required.Reader[F, ?, A]

    sealed trait Writer[-F, +B, -C] extends Value.Writer[F, B, C]:
      override def contramap[D](f: D => C): Value.Required.Writer[F, B, D]
      override def update(f: Metadata => Metadata): Value.Required.Writer[F, B, C]

    object Writer:
      type Via[F, A] = Value.Required.Writer[F, ?, A]

  sealed trait Reader[-F, +B, +C] extends Schema.Reader[F, B, C]:
    override def map[D](f: C => D): Value.Reader[F, B, D]
    override def optional: Value.Reader[F, B, Option[C]]
    override def update(f: Metadata => Metadata): Value.Reader[F, B, C]

  object Reader:
    type Via[F, A] = Value.Reader[F, ?, A]

  sealed trait Writer[-F, +B, -C] extends Schema.Writer[F, B, C]:
    override def contramap[D](f: D => C): Value.Writer[F, B, D]
    override def optional: Value.Writer[F, B, Option[C]]
    override def update(f: Metadata => Metadata): Value.Writer[F, B, C]

  object Writer:
    type Via[F, A] = Value.Writer[F, ?, A]

sealed trait Collection[-F, +B, C] extends Schema[F, B, C], Collection.Reader[F, B, C], Collection.Writer[F, B, C]:
  final override def imap[D](f: C => D)(g: D => C): Collection[F, B, D] = ivalidate(Validation.lift(f))(g)
  final def ivalidate[D, E](validation: SchemaValidation.Collection[C, D, E])(f: E => C): Collection[F, B, E] =
    Collection.Transform(this, validation, f)
  final override def optional: Collection[F, B, Option[C]] = Collection.Optional(this)
  override def schema: Schema[F, ?, ?]
  override def update(f: Metadata => Metadata): Collection[F, B, C]

object Collection:
  sealed trait Reader[-F, +B, +C] extends Schema.Reader[F, B, C]:
    def constraints: Chain[Constraint.Collection]
    final override def map[D](f: C => D): Collection.Reader[F, B, D] = validate(Validation.lift(f))
    override def optional: Collection.Reader[F, B, Option[C]] = Reader.Optional(this)
    def schema: Schema.Reader[?, ?, ?]
    override def update(f: Metadata => Metadata): Collection.Reader[F, B, C]
    final def validate[C1 >: C, D, E](validation: SchemaValidation.Collection[C1, D, E]): Collection.Reader[F, B, E] =
      Reader.Transform(this, validation)

  object Reader:
    final case class Optional[F, B, C](self: Collection.Reader[F, B, C]) extends Collection.Reader[F, B, Option[C]]:
      export self.{constraints, metadata, schema}
      override def update(f: Metadata => Metadata): Collection.Reader[F, B, Option[C]] = copy(self = self.update(f))

    final case class Root[F, +B <: Schema.Reader[F, ?, C], C](metadata: Metadata, schema: B)
        extends Collection.Reader[F, B, Vector[C]]:
      override def constraints: Chain[Constraint.Collection] = Chain.empty
      override def update(f: Metadata => Metadata): Collection.Reader[F, B, Vector[C]] = copy(metadata = f(metadata))

    final case class Transform[F, B, C, D, E](
        self: Collection.Reader[F, B, C],
        validation: SchemaValidation.Collection[C, D, E]
    ) extends Collection.Reader[F, B, E]:
      export self.{metadata, schema}
      override def constraints: Chain[Constraint.Collection] = self.constraints ++ validation.constraints
      override def update(f: Metadata => Metadata): Collection.Reader[F, B, E] = copy(self = self.update(f))

  sealed trait Writer[-F, +B, -C] extends Schema.Writer[F, B, C]:
    final def contramap[D](f: D => C): Collection.Writer[F, B, D] = Writer.Transform(this, f)
    def optional: Collection.Writer[F, B, Option[C]] = Writer.Optional(this)
    def schema: Schema.Writer[F, ?, ?]
    override def update(f: Metadata => Metadata): Collection.Writer[F, B, C]

  object Writer:
    final case class Transform[F, B, C, D](
        self: Collection.Writer[F, B, C],
        f: D => C
    ) extends Collection.Writer[F, B, D]:
      export self.{metadata, schema}
      override def update(f: Metadata => Metadata): Collection.Writer[F, B, D] = copy(self = self.update(f))

    final case class Optional[F, B, C](self: Collection.Writer[F, B, C]) extends Collection.Writer[F, B, Option[C]]:
      export self.{metadata, schema}
      override def update(f: Metadata => Metadata): Collection.Writer[F, B, Option[C]] = copy(self = self.update(f))

    final case class Root[F, +B <: Schema.Writer[F, ?, C], C](metadata: Metadata, schema: B)
        extends Collection.Writer[F, B, Vector[C]]:
      override def update(f: Metadata => Metadata): Writer[F, B, Vector[C]] = copy(metadata = f(metadata))

  final case class Optional[F, B, C](self: Collection[F, B, C]) extends Collection[F, B, Option[C]]:
    export self.{constraints, metadata, schema}
    override def update(f: Metadata => Metadata): Collection[F, B, Option[C]] = copy(self = self.update(f))

  final case class Root[F, +B <: Schema[F, ?, C], C](metadata: Metadata, schema: B) extends Collection[F, B, Vector[C]]:
    override def constraints: Chain[Constraint.Collection] = Chain.empty
    override def update(f: Metadata => Metadata): Collection[F, B, Vector[C]] = copy(metadata = f(metadata))

  final case class Transform[F, B, C, D, E](
      self: Collection[F, B, C],
      validation: SchemaValidation.Collection[C, D, E],
      f: E => C
  ) extends Collection[F, B, E]:
    export self.{metadata, schema}
    override def constraints: Chain[Constraint.Collection] = self.constraints ++ validation.constraints
    override def update(f: Metadata => Metadata): Collection[F, B, E] = copy(self = self.update(f))

sealed trait Dictionary[-F, +B, C] extends Schema[F, B, C], Dictionary.Reader[F, B, C], Dictionary.Writer[F, B, C]:
  final override def imap[D](f: C => D)(g: D => C): Dictionary[F, B, D] = Dictionary.Transform(this, f, g)
  final override def optional: Dictionary[F, B, Option[C]] = Dictionary.Optional(this)
  override def update(f: Metadata => Metadata): Dictionary[F, B, C]

object Dictionary:
  sealed trait Reader[-F, +B, +C] extends Schema.Reader[F, B, C]:
    final override def map[D](f: C => D): Dictionary.Reader[F, B, D] = Reader.Transform(this, f)
    override def optional: Dictionary.Reader[F, B, Option[C]] = Reader.Optional(this)
    override def update(f: Metadata => Metadata): Dictionary.Reader[F, B, C]

  object Reader:
    final case class Optional[F, B, C](self: Dictionary.Reader[F, B, C]) extends Dictionary.Reader[F, B, Option[C]]:
      export self.metadata
      override def update(f: Metadata => Metadata): Dictionary.Reader[F, B, Option[C]] = copy(self = self.update(f))

    final case class Root[F, B, +C <: Schema.Reader[F, ?, D], D](
        metadata: Metadata,
        key: Primitive.Required.Reader[B],
        value: C
    ) extends Dictionary.Reader[F, C, List[(B, D)]]:
      override def update(f: Metadata => Metadata): Dictionary.Reader[F, C, List[(B, D)]] = copy(metadata = f(metadata))

    final case class Transform[F, B, C, D](self: Dictionary.Reader[F, B, C], f: C => D)
        extends Dictionary.Reader[F, B, D]:
      export self.metadata
      override def update(f: Metadata => Metadata): Dictionary.Reader[F, B, D] = copy(self = self.update(f))

  sealed trait Writer[-F, +B, -C] extends Schema.Writer[F, B, C]:
    override def contramap[D](f: D => C): Dictionary.Writer[F, B, D] = Writer.Transform(this, f)
    override def optional: Dictionary.Writer[F, B, Option[C]] = Writer.Optional(this)
    override def update(f: Metadata => Metadata): Dictionary.Writer[F, B, C]

  object Writer:
    final case class Optional[F, B, C](self: Dictionary.Writer[F, B, C]) extends Dictionary.Writer[F, B, Option[C]]:
      export self.metadata
      override def update(f: Metadata => Metadata): Dictionary.Writer[F, B, Option[C]] = copy(self = self.update(f))

    final case class Root[F, B, +C <: Schema.Writer[F, ?, D], D](
        metadata: Metadata,
        key: Primitive.Required.Writer[B],
        value: C
    ) extends Dictionary.Writer[F, C, List[(B, D)]]:
      override def update(f: Metadata => Metadata): Dictionary.Writer[F, C, List[(B, D)]] = copy(metadata = f(metadata))

    final case class Transform[F, B, C, D](self: Dictionary.Writer[F, B, C], f: D => C)
        extends Dictionary.Writer[F, B, D]:
      export self.metadata
      override def update(f: Metadata => Metadata): Dictionary.Writer[F, B, D] = copy(self = self.update(f))

  final case class Optional[F, B, C](self: Dictionary[F, B, C]) extends Dictionary[F, B, Option[C]]:
    export self.metadata
    override def update(f: Metadata => Metadata): Dictionary[F, B, Option[C]] = copy(self = self.update(f))

  final case class Root[F, B, +C <: Schema[F, ?, D], D](metadata: Metadata, key: Primitive.Required[B], value: C)
      extends Dictionary[F, C, List[(B, D)]]:
    override def update(f: Metadata => Metadata): Dictionary[F, C, List[(B, D)]] = copy(metadata = f(metadata))

  final case class Transform[F, B, C, D](self: Dictionary[F, B, C], f: C => D, g: D => C) extends Dictionary[F, B, D]:
    export self.metadata
    override def update(f: Metadata => Metadata): Dictionary[F, B, D] = copy(self = self.update(f))

sealed trait Dynamic[F, B] extends Schema[F, Nothing, B], Dynamic.Reader[F, B], Dynamic.Writer[F, B]:
  override def imap[C](f: B => C)(g: C => B): Dynamic[F, C] = Dynamic.Transform(this, f, g)
  override def optional: Dynamic[F, Option[B]] = Dynamic.Optional(this)
  override def update(f: Metadata => Metadata): Dynamic[F, B]

object Dynamic:
  sealed trait Reader[-F, +B] extends Schema.Reader[F, Nothing, B]:
    final override def map[C](f: B => C): Dynamic.Reader[F, C] = Reader.Transform(this, f)
    override def optional: Dynamic.Reader[F, Option[B]] = Reader.Optional(this)
    override def update(f: Metadata => Metadata): Dynamic.Reader[F, B]

  object Reader:
    final case class Optional[F, B](self: Dynamic.Reader[F, B]) extends Dynamic.Reader[F, Option[B]]:
      export self.metadata
      override def update(f: Metadata => Metadata): Dynamic.Reader[F, Option[B]] =
        copy(self = self.update(f))

    final case class Root[F](metadata: Metadata) extends Dynamic.Reader[F, F]:
      override def update(f: Metadata => Metadata): Dynamic.Reader[F, F] =
        copy(metadata = f(metadata))

    final case class Transform[F, B, C](self: Dynamic.Reader[F, B], f: B => C) extends Dynamic.Reader[F, C]:
      export self.metadata
      override def update(f: Metadata => Metadata): Dynamic.Reader[F, C] = copy(self = self.update(f))

  sealed trait Writer[F, -A] extends Schema.Writer[F, Nothing, A]:
    final override def contramap[B](f: B => A): Dynamic.Writer[F, B] = Writer.Transform(this, f)
    override def optional: Dynamic.Writer[F, Option[A]] = Writer.Optional(this)
    override def update(f: Metadata => Metadata): Dynamic.Writer[F, A]

  object Writer:
    final case class Optional[F, B](self: Dynamic.Writer[F, B]) extends Dynamic.Writer[F, Option[B]]:
      export self.metadata
      override def update(f: Metadata => Metadata): Dynamic.Writer[F, Option[B]] = copy(self = self.update(f))

    final case class Root[F](metadata: Metadata) extends Dynamic.Writer[F, F]:
      override def update(f: Metadata => Metadata): Dynamic.Writer[F, F] = copy(metadata = f(metadata))

    final case class Transform[F, B, C](self: Dynamic.Writer[F, B], f: C => B) extends Dynamic.Writer[F, C]:
      export self.metadata
      override def update(f: Metadata => Metadata): Dynamic.Writer[F, C] = copy(self = self.update(f))

  final case class Optional[F, B](self: Dynamic[F, B]) extends Dynamic[F, Option[B]]:
    export self.metadata
    override def update(f: Metadata => Metadata): Dynamic[F, Option[B]] = copy(self = self.update(f))

  final case class Root[F](metadata: Metadata) extends Dynamic[F, F]:
    override def update(f: Metadata => Metadata): Dynamic[F, F] = copy(metadata = f(metadata))

  final case class Transform[F, B, C](self: Dynamic[F, B], f: B => C, g: C => B) extends Dynamic[F, C]:
    export self.metadata
    override def update(f: Metadata => Metadata): Dynamic[F, C] = copy(self = self.update(f))

sealed trait Enumeration[-F, +B, C] extends Value[F, B, C], Enumeration.Reader[F, B, C], Enumeration.Writer[F, B, C]:
  override def imap[D](f: C => D)(g: D => C): Enumeration[F, B, D] = Enumeration.Transform(this, f, g)
  override def optional: Enumeration[F, B, Option[C]] = Enumeration.Optional(this)
  override def update(f: Metadata => Metadata): Enumeration[F, B, C]

object Enumeration:
  sealed trait Required[-F, +B, C]
      extends Value.Required[F, B, C],
        Enumeration[F, B, C],
        Enumeration.Required.Reader[F, B, C],
        Enumeration.Required.Writer[F, B, C]:
    override def imap[D](f: C => D)(g: D => C): Enumeration.Required[F, B, D] = Required.Transform(this, f, g)
    override def update(f: Metadata => Metadata): Enumeration.Required[F, B, C]

  object Required:
    sealed trait Reader[-F, +B, +C] extends Value.Required.Reader[F, B, C], Enumeration.Reader[F, B, C]:
      override def map[D](f: C => D): Enumeration.Required.Reader[F, B, D] = Reader.Transform(this, f)
      override def update(f: Metadata => Metadata): Enumeration.Required.Reader[F, B, C]

    object Reader:
      type Via[F, A] = Enumeration.Required.Reader[F, ?, A]

      final case class Root[F, +B <: Value.Required.Reader[F, ?, C], C, D](
          metadata: Metadata,
          schema: B,
          mapping: Mapping[D, C],
          writer: Schema.Writer[F, ?, C]
      ) extends Enumeration.Required.Reader[F, B, D]:
        override def update(f: Metadata => Metadata): Enumeration.Required.Reader[F, B, D] =
          copy(metadata = f(metadata))

      final case class Transform[F, B, C, D](self: Enumeration.Required.Reader[F, B, C], f: C => D)
          extends Enumeration.Required.Reader[F, B, D]:
        export self.metadata
        override def update(f: Metadata => Metadata): Enumeration.Required.Reader[F, B, D] =
          copy(self = self.update(f))

    sealed trait Writer[-F, +B, -C] extends Value.Required.Writer[F, B, C], Enumeration.Writer[F, B, C]:
      override def contramap[D](f: D => C): Enumeration.Required.Writer[F, B, D] = Writer.Transform(this, f)
      override def update(f: Metadata => Metadata): Enumeration.Required.Writer[F, B, C]

    object Writer:
      type Via[F, A] = Enumeration.Required.Writer[F, ?, A]

      final case class Root[F, +B <: Value.Required.Writer[F, ?, C], C, D](metadata: Metadata, schema: B, f: D => C)
          extends Enumeration.Required.Writer[F, B, D]:
        override def update(f: Metadata => Metadata): Enumeration.Required.Writer[F, B, D] =
          copy(metadata = f(metadata))

      final case class Transform[F, B, C, D](self: Enumeration.Required.Writer[F, B, C], f: D => C)
          extends Enumeration.Required.Writer[F, B, D]:
        export self.metadata
        override def update(f: Metadata => Metadata): Enumeration.Required.Writer[F, B, D] =
          copy(self = self.update(f))

    final case class Transform[F, B, C, D](self: Enumeration.Required[F, B, C], f: C => D, g: D => C)
        extends Enumeration.Required[F, B, D]:
      export self.metadata
      override def update(f: Metadata => Metadata): Enumeration.Required[F, B, D] = copy(self = self.update(f))

  sealed trait Reader[-F, +B, +C] extends Value.Reader[F, B, C]:
    override def map[D](f: C => D): Enumeration.Reader[F, B, D] = Reader.Transform(this, f)
    override def optional: Enumeration.Reader[F, B, Option[C]] = Reader.Optional(this)
    override def update(f: Metadata => Metadata): Enumeration.Reader[F, B, C]

  object Reader:
    final case class Optional[F, B, C](self: Enumeration.Reader[F, B, C]) extends Enumeration.Reader[F, B, Option[C]]:
      export self.metadata
      override def update(f: Metadata => Metadata): Enumeration.Reader[F, B, Option[C]] = copy(self = self.update(f))

    final case class Root[F, +B <: Value.Reader[F, ?, C], C, D](
        metadata: Metadata,
        schema: B,
        mapping: Mapping[D, C],
        writer: Schema.Writer[F, ?, C]
    ) extends Enumeration.Reader[F, B, D]:
      override def update(f: Metadata => Metadata): Enumeration.Reader[F, B, D] =
        copy(metadata = f(metadata))

    final case class Transform[F, B, C, D](self: Enumeration.Reader[F, B, C], f: C => D)
        extends Enumeration.Reader[F, B, D]:
      export self.metadata
      override def update(f: Metadata => Metadata): Enumeration.Reader[F, B, D] =
        copy(self = self.update(f))

  sealed trait Writer[-F, +B, -C] extends Value.Writer[F, B, C]:
    override def contramap[D](f: D => C): Enumeration.Writer[F, B, D] = Writer.Transform(this, f)
    override def optional: Enumeration.Writer[F, B, Option[C]] = Writer.Optional(this)
    override def update(f: Metadata => Metadata): Enumeration.Writer[F, B, C]

  object Writer:
    type Via[F, A] = Enumeration.Writer[F, ?, A]

    final case class Optional[F, B, C](self: Enumeration.Writer[F, B, C]) extends Enumeration.Writer[F, B, Option[C]]:
      export self.metadata
      override def update(f: Metadata => Metadata): Enumeration.Writer[F, B, Option[C]] =
        copy(self = self.update(f))

    final case class Transform[F, B, C, D](self: Enumeration.Writer[F, B, C], f: D => C)
        extends Enumeration.Writer[F, B, D]:
      export self.metadata
      override def update(f: Metadata => Metadata): Enumeration.Writer[F, B, D] =
        copy(self = self.update(f))

  final case class Optional[F, B, C](self: Enumeration[F, B, C]) extends Enumeration[F, B, Option[C]]:
    export self.metadata
    override def update(f: Metadata => Metadata): Enumeration[F, B, Option[C]] = copy(self = self.update(f))

  final case class Root[F, +B <: Value[F, ?, C], C, D](metadata: Metadata, schema: B, mapping: Mapping[D, C])
      extends Enumeration[F, B, D]:
    override def update(f: Metadata => Metadata): Enumeration[F, B, D] = copy(metadata = f(metadata))

  final case class Transform[F, B, C, D](self: Enumeration[F, B, C], f: C => D, g: D => C) extends Enumeration[F, B, D]:
    export self.metadata
    override def update(f: Metadata => Metadata): Enumeration[F, B, D] = copy(self = self.update(f))

sealed trait Primitive[A] extends Value[Any, Nothing, A], Primitive.Reader[A], Primitive.Writer[A]:
  override def imap[B](f: A => B)(g: B => A): Primitive[B] = ivalidate(Validation.lift(f))(g)
  def ivalidate[B, C, D](validation: SchemaValidation.Primitive[A, B, C, D])(
      f: D => A
  ): Primitive[D] = Primitive.Transform(this, validation, f)
  final override def optional: Primitive[Option[A]] = Primitive.Optional(this)
  override def update(f: Metadata => Metadata): Primitive[A]

object Primitive:
  sealed trait Required[A]
      extends Value.Required[Any, Nothing, A],
        Primitive[A],
        Primitive.Required.Reader[A],
        Primitive.Required.Writer[A]:
    final override def imap[C](f: A => C)(g: C => A): Primitive.Required[C] = ivalidate(Validation.lift(f))(g)
    override def ivalidate[B, C, D](validation: SchemaValidation.Primitive[A, B, C, D])(
        f: D => A
    ): Primitive.Required[D] = Required.Transform(this, validation, f)
    override def update(f: Metadata => Metadata): Primitive.Required[A]

  object Required:
    sealed trait Reader[+A] extends Value.Required.Reader[Any, Nothing, A], Primitive.Reader[A]:
      final override def map[C](f: A => C): Primitive.Required.Reader[C] = validate(Validation.lift(f))
      final override def validate[F1 >: A, B, C, D](
          transformation: SchemaValidation.Primitive[F1, B, C, D]
      ): Primitive.Required.Reader[D] = Reader.Transform(this, transformation)
      override def update(f: Metadata => Metadata): Primitive.Required.Reader[A]

    object Reader:
      final case class Transform[A, B, C, D](
          self: Primitive.Required.Reader[A],
          validation: SchemaValidation.Primitive[A, B, C, D]
      ) extends Primitive.Required.Reader[D]:
        export self.{metadata, tpe}
        override def constraints: Chain[Constraint.Primitive[?]] = self.constraints ++ validation.constraints
        override def update(f: Metadata => Metadata): Primitive.Required.Reader[D] = copy(self = self.update(f))

    sealed trait Writer[-A] extends Value.Required.Writer[Any, Nothing, A], Primitive.Writer[A]:
      final override def contramap[B](f: B => A): Primitive.Required.Writer[B] = Writer.Transform(this, f)
      override def update(f: Metadata => Metadata): Primitive.Required.Writer[A]

    object Writer:
      final case class Transform[A, B](self: Primitive.Required.Writer[A], f: B => A)
          extends Primitive.Required.Writer[B]:
        export self.{metadata, tpe}
        override def update(f: Metadata => Metadata): Primitive.Required.Writer[B] = copy(self = self.update(f))

    final case class Root[A](metadata: Metadata, tpe: Type[A]) extends Primitive.Required[A]:
      override def constraints: Chain[Constraint.Primitive[?]] = Chain.empty
      override def update(f: Metadata => Metadata): Primitive.Required[A] = copy(metadata = f(metadata))

    final case class Transform[A, B, C, D](
        self: Primitive.Required[A],
        validation: SchemaValidation.Primitive[A, B, C, D],
        f: D => A
    ) extends Primitive.Required[D]:
      export self.{metadata, tpe}
      override def constraints: Chain[Constraint.Primitive[?]] = self.constraints ++ validation.constraints
      override def update(f: Metadata => Metadata): Primitive.Required[D] = copy(self = self.update(f))

  sealed trait Reader[+A] extends Value.Reader[Any, Nothing, A]:
    def constraints: Chain[Constraint.Primitive[?]]
    override def map[C](f: A => C): Primitive.Reader[C] = validate(Validation.lift(f))
    override def optional: Primitive.Reader[Option[A]] = Reader.Optional(this)
    def tpe: Type[?]
    def validate[A1 >: A, B, C, D](
        validation: SchemaValidation.Primitive[A1, B, C, D]
    ): Primitive.Reader[D] = Reader.Transform(this, validation)
    override def update(f: Metadata => Metadata): Primitive.Reader[A]

  object Reader:
    final case class Transform[A, B, C, D](
        self: Primitive.Reader[A],
        validation: SchemaValidation.Primitive[A, B, C, D]
    ) extends Primitive.Reader[D]:
      export self.{metadata, tpe}
      override def constraints: Chain[Constraint.Primitive[?]] = self.constraints ++ validation.constraints
      override def update(f: Metadata => Metadata): Primitive.Reader[D] = copy(self = self.update(f))

    final case class Optional[A](self: Primitive.Reader[A]) extends Primitive.Reader[Option[A]]:
      export self.{constraints, metadata, tpe}
      override def update(f: Metadata => Metadata): Primitive.Reader[Option[A]] = copy(self = self.update(f))

  sealed trait Writer[-A] extends Value.Writer[Any, Nothing, A]:
    override def contramap[B](f: B => A): Primitive.Writer[B] = Writer.Transform(this, f)
    override def optional: Primitive.Writer[Option[A]] = Writer.Optional(this)
    def tpe: Type[?]
    override def update(f: Metadata => Metadata): Primitive.Writer[A]

  object Writer:
    final case class Transform[A, B](self: Primitive.Writer[A], f: B => A) extends Primitive.Writer[B]:
      export self.{metadata, tpe}
      override def update(f: Metadata => Metadata): Primitive.Writer[B] = copy(self = self.update(f))

    final case class Optional[A](self: Primitive.Writer[A]) extends Primitive.Writer[Option[A]]:
      export self.{metadata, tpe}
      override def update(f: Metadata => Metadata): Primitive.Writer[Option[A]] = copy(self = self.update(f))

  final case class Optional[A](self: Primitive[A]) extends Primitive[Option[A]]:
    export self.{constraints, metadata, tpe}
    override def update(f: Metadata => Metadata): Primitive[Option[A]] = copy(self = self.update(f))

  final case class Transform[A, B, C, D](
      self: Primitive[A],
      validation: SchemaValidation.Primitive[A, B, C, D],
      f: D => A
  ) extends Primitive[D]:
    export self.{metadata, tpe}
    override def constraints: Chain[Constraint.Primitive[?]] = self.constraints ++ validation.constraints
    override def update(f: Metadata => Metadata): Primitive[D] = copy(self = self.update(f))

sealed trait Product[-F, +B, C] extends Schema[F, B, C], Product.Reader[F, B, C], Product.Writer[F, B, C]:
  override def imap[D](f: C => D)(g: D => C): Product[F, B, D] = Product.Transform(this, f, g)
  override def optional: Product[F, B, Option[C]] = Product.Optional(this)
  def productWith[F1 <: F, D, E](merge: (Metadata, Metadata) => Metadata)(
      product: Product[F1, D, E]
  ): Product[F1, B & D, (C, E)] =
    Product.Combine(merge(metadata, product.metadata), this, product)
  def schemas: Chain[Schema[F, ?, ?]]
  override def update(f: Metadata => Metadata): Product[F, B, C]

object Product:
  sealed trait Reader[-F, +B, +C] extends Schema.Reader[F, B, C]:
    override def map[D](f: C => D): Product.Reader[F, B, D] = Reader.Transform(this, f)
    override def optional: Product.Reader[F, B, Option[C]] = Reader.Optional(this)
    def productWith[F1 <: F, D, E](merge: (Metadata, Metadata) => Metadata)(
        product: Product.Reader[F1, D, E]
    ): Product.Reader[F1, B & D, (C, E)] =
      Reader.Combine(merge(metadata, product.metadata), this, product)
    def schemas: Chain[Schema.Reader[F, ?, ?]]
    override def update(f: Metadata => Metadata): Product.Reader[F, B, C]

  object Reader:
    final case class Combine[F, B, C, D, E](
        metadata: Metadata,
        left: Product.Reader[F, B, C],
        right: Product.Reader[F, D, E]
    ) extends Product.Reader[F, B & D, (C, E)]:
      override def schemas: Chain[Schema.Reader[F, ?, ?]] = left.schemas ++ right.schemas
      override def update(f: Metadata => Metadata): Product.Reader[F, B & D, (C, E)] = copy(metadata = f(metadata))

    final case class One[F, +B <: Schema.Reader[F, ?, C], C](metadata: Metadata, schema: B)
        extends Product.Reader[F, B, C]:
      override def schemas: Chain[Schema.Reader[F, ?, ?]] = Chain.one(schema)
      override def update(f: Metadata => Metadata): Product.Reader[F, B, C] = copy(metadata = f(metadata))

    final case class Optional[F, B, C](self: Product.Reader[F, B, C]) extends Product.Reader[F, B, Option[C]]:
      export self.{metadata, schemas}
      override def update(f: Metadata => Metadata): Product.Reader[F, B, Option[C]] = copy(self = self.update(f))

    final case class Transform[F, B, C, D](self: Product.Reader[F, B, C], f: C => D) extends Product.Reader[F, B, D]:
      export self.{metadata, schemas}
      override def update(f: Metadata => Metadata): Product.Reader[F, B, D] = copy(self = self.update(f))

  sealed trait Writer[-F, +B, -C] extends Schema.Writer[F, B, C]:
    override def contramap[D](f: D => C): Product.Writer[F, B, D] = Writer.Transform(this, f)
    override def optional: Product.Writer[F, B, Option[C]] = Writer.Optional(this)
    def productWith[F1 <: F, D, E](merge: (Metadata, Metadata) => Metadata)(
        product: Product.Writer[F1, D, E]
    ): Product.Writer[F1, B & D, (C, E)] =
      Writer.Combine(merge(metadata, product.metadata), this, product)
    def schemas: Chain[Schema.Writer[F, ?, ?]]
    override def update(f: Metadata => Metadata): Product.Writer[F, B, C]

  object Writer:
    final case class Combine[F, B, C, D, E](
        metadata: Metadata,
        left: Product.Writer[F, B, C],
        right: Product.Writer[F, D, E]
    ) extends Product.Writer[F, B & D, (C, E)]:
      override def schemas: Chain[Schema.Writer[F, ?, ?]] = left.schemas ++ right.schemas
      override def update(f: Metadata => Metadata): Product.Writer[F, B & D, (C, E)] = copy(metadata = f(metadata))

    final case class One[F, +B <: Schema.Writer[F, ?, C], C](metadata: Metadata, schema: B)
        extends Product.Writer[F, B, C]:
      override def schemas: Chain[Schema.Writer[F, ?, ?]] = Chain.one(schema)
      override def update(f: Metadata => Metadata): Product.Writer[F, B, C] = copy(metadata = f(metadata))

    final case class Optional[F, B, C](self: Product.Writer[F, B, C]) extends Product.Writer[F, B, Option[C]]:
      export self.{metadata, schemas}
      override def update(f: Metadata => Metadata): Product.Writer[F, B, Option[C]] = copy(self = self.update(f))

    final case class Transform[F, B, C, D](self: Product.Writer[F, B, C], f: D => C) extends Product.Writer[F, B, D]:
      export self.{metadata, schemas}
      override def update(f: Metadata => Metadata): Product.Writer[F, B, D] = copy(self = self.update(f))

  final case class Combine[F, B, C, D, E](metadata: Metadata, left: Product[F, B, C], right: Product[F, D, E])
      extends Product[F, B & D, (C, E)]:
    override def schemas: Chain[Schema[F, ?, ?]] = left.schemas ++ right.schemas
    override def update(f: Metadata => Metadata): Product[F, B & D, (C, E)] = copy(metadata = f(metadata))

  case class Empty(metadata: Metadata) extends Product[Any, Nothing, Unit]:
    override def schemas: Chain[Nothing] = Chain.empty
    override def update(f: Metadata => Metadata): Product[Any, Nothing, Unit] = copy(metadata = f(metadata))

  final case class One[F, +B <: Schema[F, ?, C], C](metadata: Metadata, schema: B) extends Product[F, B, C]:
    override def schemas: Chain[Schema[F, ?, ?]] = Chain.one(schema)
    override def update(f: Metadata => Metadata): Product[F, B, C] = copy(metadata = f(metadata))

  final case class Optional[F, B, C](self: Product[F, B, C]) extends Product[F, B, Option[C]]:
    export self.{metadata, schemas}
    override def update(f: Metadata => Metadata): Product[F, B, Option[C]] = copy(self = self.update(f))

  final case class Transform[F, B, C, D](self: Product[F, B, C], f: C => D, g: D => C) extends Product[F, B, D]:
    export self.{metadata, schemas}
    override def update(f: Metadata => Metadata): Product[F, B, D] = copy(self = self.update(f))

sealed trait Record[-F, +B, C] extends Schema[F, B, C], Record.Reader[F, B, C], Record.Writer[F, B, C]:
  def fields: Chain[Field[F, ?, ?]]
  override def imap[D](f: C => D)(g: D => C): Record[F, B, D] = Record.Transform(this, f, g)
  override def optional: Record[F, B, Option[C]] = Record.Optional(this)
  def productWith[F1 <: F, D, E](merge: (Metadata, Metadata) => Metadata)(
      product: Record[F1, D, E]
  ): Record[F1, B & D, (C, E)] = Record.Combine(merge(metadata, product.metadata), this, product)
  override def update(f: Metadata => Metadata): Record[F, B, C]

object Record:
  sealed trait Reader[-F, +B, +C] extends Schema.Reader[F, B, C]:
    def fields: Chain[Field.Reader[F, ?, ?]]
    override def map[D](f: C => D): Record.Reader[F, B, D] = Reader.Transform(this, f)
    override def optional: Record.Reader[F, B, Option[C]] = Reader.Optional(this)
    def productWith[F1 <: F, D, E](merge: (Metadata, Metadata) => Metadata)(
        product: Record.Reader[F1, D, E]
    ): Record.Reader[F1, B & D, (C, E)] = Reader.Combine(merge(metadata, product.metadata), this, product)
    override def update(f: Metadata => Metadata): Record.Reader[F, B, C]

  object Reader:
    final case class Combine[F, B, C, D, E](
        metadata: Metadata,
        left: Record.Reader[F, B, C],
        right: Record.Reader[F, D, E]
    ) extends Record.Reader[F, B & D, (C, E)]:
      override def fields: Chain[Field.Reader[F, ?, ?]] = left.fields ++ right.fields
      override def update(f: Metadata => Metadata): Record.Reader[F, B & D, (C, E)] = copy(metadata = f(metadata))

    final case class One[F, B, C](metadata: Metadata, field: Field.Reader[F, B, C]) extends Record.Reader[F, B, C]:
      override def fields: Chain[Field.Reader[F, ?, ?]] = Chain.one(field)
      override def update(f: Metadata => Metadata): Record.Reader[F, B, C] = copy(metadata = f(metadata))

    final case class Optional[F, B, C](self: Record.Reader[F, B, C]) extends Record.Reader[F, B, Option[C]]:
      export self.{fields, metadata}
      override def update(f: Metadata => Metadata): Record.Reader[F, B, Option[C]] = copy(self = self.update(f))

    final case class Transform[F, B, C, D](self: Record.Reader[F, B, C], f: C => D) extends Record.Reader[F, B, D]:
      export self.{fields, metadata}
      override def update(f: Metadata => Metadata): Record.Reader[F, B, D] = copy(self = self.update(f))

  sealed trait Writer[-F, +B, -C] extends Schema.Writer[F, B, C]:
    override def contramap[D](f: D => C): Record.Writer[F, B, D] = Writer.Transform(this, f)
    def fields: Chain[Field.Writer[F, ?, ?]]
    override def optional: Record.Writer[F, B, Option[C]] = Writer.Optional(this)
    def productWith[F1 <: F, D, E](merge: (Metadata, Metadata) => Metadata)(
        product: Record.Writer[F1, D, E]
    ): Record.Writer[F1, B & D, (C, E)] = Writer.Combine(merge(metadata, product.metadata), this, product)
    override def update(f: Metadata => Metadata): Record.Writer[F, B, C]

  object Writer:
    final case class Combine[F, B, C, D, E](
        metadata: Metadata,
        left: Record.Writer[F, B, C],
        right: Record.Writer[F, D, E]
    ) extends Record.Writer[F, B & D, (C, E)]:
      override def fields: Chain[Field.Writer[F, ?, ?]] = left.fields ++ right.fields
      override def update(f: Metadata => Metadata): Record.Writer[F, B & D, (C, E)] = copy(metadata = f(metadata))

    final case class One[F, B, C](metadata: Metadata, field: Field.Writer[F, B, C]) extends Record.Writer[F, B, C]:
      override def fields: Chain[Field.Writer[F, ?, ?]] = Chain.one(field)
      override def update(f: Metadata => Metadata): Record.Writer[F, B, C] = copy(metadata = f(metadata))

    final case class Optional[F, B, C](self: Record.Writer[F, B, C]) extends Record.Writer[F, B, Option[C]]:
      export self.{fields, metadata}
      override def update(f: Metadata => Metadata): Record.Writer[F, B, Option[C]] = copy(self = self.update(f))

    final case class Transform[F, B, C, D](self: Record.Writer[F, B, C], f: D => C) extends Record.Writer[F, B, D]:
      export self.{fields, metadata}
      override def update(f: Metadata => Metadata): Record.Writer[F, B, D] = copy(self = self.update(f))

  final case class Empty(metadata: Metadata) extends Record[Any, Nothing, Unit]:
    override def fields: Chain[Nothing] = Chain.empty
    override def update(f: Metadata => Metadata): Record[Any, Nothing, Unit] = copy(metadata = f(metadata))

  final case class Combine[F, B, C, D, E](metadata: Metadata, left: Record[F, B, C], right: Record[F, D, E])
      extends Record[F, B & D, (C, E)]:
    override def fields: Chain[Field[F, ?, ?]] = left.fields ++ right.fields
    override def update(f: Metadata => Metadata): Record[F, B & D, (C, E)] = copy(metadata = f(metadata))

  final case class One[F, B, C](metadata: Metadata, field: Field[F, B, C]) extends Record[F, B, C]:
    override def fields: Chain[Field[F, ?, ?]] = Chain.one(field)
    override def update(f: Metadata => Metadata): Record[F, B, C] = copy(metadata = f(metadata))

  final case class Optional[F, B, C](self: Record[F, B, C]) extends Record[F, B, Option[C]]:
    export self.{fields, metadata}
    override def update(f: Metadata => Metadata): Record[F, B, Option[C]] = copy(self = self.update(f))

  final case class Transform[F, B, C, D](self: Record[F, B, C], f: C => D, g: D => C) extends Record[F, B, D]:
    export self.{fields, metadata}
    override def update(f: Metadata => Metadata): Record[F, B, D] = copy(self = self.update(f))

sealed trait Sum[-F, +B, C] extends Schema[F, B, C], Sum.Reader[F, B, C], Sum.Writer[F, B, C]:
  override def branches: NonEmptyChain[Branch[F, ?, ?]]
  override def imap[D](f: C => D)(g: D => C): Sum[F, B, D] = Sum.Transform(this, f, g)
  override def optional: Sum[F, B, Option[C]] = Sum.Optional(this)
  def orElseWith[F1 <: F, D, E](merge: (Metadata, Metadata) => Metadata)(
      sum: Sum[F1, D, E]
  ): Sum[F1, B | D, Either[C, E]] =
    Sum.Combine(merge(metadata, sum.metadata), this, sum)
  override def update(f: Metadata => Metadata): Sum[F, B, C]

object Sum:
  sealed trait Reader[-F, +B, +C] extends Schema.Reader[F, B, C]:
    def branches: NonEmptyChain[Branch.Reader[F, ?, ?]]
    final override def map[D](f: C => D): Sum.Reader[F, B, D] = Reader.Transform(this, f)
    override def optional: Sum.Reader[F, B, Option[C]] = Reader.Optional(this)
    def orElseWith[F1 <: F, D, E](merge: (Metadata, Metadata) => Metadata)(
        sum: Sum.Reader[F1, D, E]
    ): Sum.Reader[F1, B | D, Either[C, E]] = Reader.Combine(merge(metadata, sum.metadata), this, sum)
    override def update(f: Metadata => Metadata): Sum.Reader[F, B, C]

  object Reader:
    final case class Combine[F, B, C, D, E](metadata: Metadata, left: Sum.Reader[F, B, C], right: Sum.Reader[F, D, E])
        extends Sum.Reader[F, B | D, Either[C, E]]:
      override def branches: NonEmptyChain[Branch.Reader[F, ?, ?]] = left.branches ++ right.branches
      override def update(f: Metadata => Metadata): Sum.Reader[F, B | D, Either[C, E]] = copy(metadata = f(metadata))

    final case class Optional[F, B, C](self: Sum.Reader[F, B, C]) extends Sum.Reader[F, B, Option[C]]:
      export self.{branches, metadata}
      override def update(f: Metadata => Metadata): Sum.Reader[F, B, Option[C]] = copy(self = self.update(f))

    final case class Root[F, B, C](metadata: Metadata, branch: Branch.Reader[F, B, C]) extends Sum.Reader[F, B, C]:
      override def branches: NonEmptyChain[Branch.Reader[F, B, C]] = NonEmptyChain.one(branch)
      override def update(f: Metadata => Metadata): Sum.Reader[F, B, C] = copy(metadata = f(metadata))

    final case class Transform[F, B, C, D](self: Sum.Reader[F, B, C], f: C => D) extends Sum.Reader[F, B, D]:
      export self.{branches, metadata}
      override def update(f: Metadata => Metadata): Sum.Reader[F, B, D] = copy(self = self.update(f))

  sealed trait Writer[-F, +B, -C] extends Schema.Writer[F, B, C]:
    def branches: NonEmptyChain[Branch.Writer[F, ?, ?]]
    final override def contramap[D](f: D => C): Sum.Writer[F, B, D] = Writer.Transform(this, f)
    override def optional: Sum.Writer[F, B, Option[C]] = Writer.Optional(this)
    def orElseWith[F1 <: F, D, E](merge: (Metadata, Metadata) => Metadata)(
        sum: Sum.Writer[F1, D, E]
    ): Sum.Writer[F1, B | D, Either[C, E]] = Writer.Combine(merge(metadata, sum.metadata), this, sum)
    override def update(f: Metadata => Metadata): Sum.Writer[F, B, C]

  object Writer:
    final case class Combine[F, B, C, D, E](metadata: Metadata, left: Sum.Writer[F, B, C], right: Sum.Writer[F, D, E])
        extends Sum.Writer[F, B | D, Either[C, E]]:
      override def branches: NonEmptyChain[Branch.Writer[F, ?, ?]] = left.branches ++ right.branches
      override def update(f: Metadata => Metadata): Sum.Writer[F, B | D, Either[C, E]] = copy(metadata = f(metadata))

    final case class Optional[F, B, C](self: Sum.Writer[F, B, C]) extends Sum.Writer[F, B, Option[C]]:
      export self.{branches, metadata}
      override def update(f: Metadata => Metadata): Sum.Writer[F, B, Option[C]] = copy(self = self.update(f))

    final case class Root[F, B, C](metadata: Metadata, branch: Branch.Writer[F, B, C]) extends Sum.Writer[F, B, C]:
      override def branches: NonEmptyChain[Branch.Writer[F, B, C]] = NonEmptyChain.one(branch)
      override def update(f: Metadata => Metadata): Sum.Writer[F, B, C] = copy(metadata = f(metadata))

    final case class Transform[F, B, C, D](self: Sum.Writer[F, B, C], f: D => C) extends Sum.Writer[F, B, D]:
      export self.{branches, metadata}
      override def update(f: Metadata => Metadata): Sum.Writer[F, B, D] = copy(self = self.update(f))

  final case class Combine[F, B, C, D, E](metadata: Metadata, left: Sum[F, B, C], right: Sum[F, D, E])
      extends Sum[F, B | D, Either[C, E]]:
    override def branches: NonEmptyChain[Branch[F, ?, ?]] = left.branches ++ right.branches
    override def update(f: Metadata => Metadata): Sum[F, B | D, Either[C, E]] = copy(metadata = f(metadata))

  final case class Optional[F, B, C](self: Sum[F, B, C]) extends Sum[F, B, Option[C]]:
    export self.{branches, metadata}
    override def update(f: Metadata => Metadata): Sum[F, B, Option[C]] = copy(self = self.update(f))

  final case class Root[F, B, C](metadata: Metadata, branch: Branch[F, B, C]) extends Sum[F, B, C]:
    override def branches: NonEmptyChain[Branch[F, B, C]] = NonEmptyChain.one(branch)
    override def update(f: Metadata => Metadata): Sum[F, B, C] = copy(metadata = f(metadata))

  final case class Transform[F, B, C, D](self: Sum[F, B, C], f: C => D, g: D => C) extends Sum[F, B, D]:
    export self.{branches, metadata}
    override def update(f: Metadata => Metadata): Sum[F, B, D] = copy(self = self.update(f))

sealed trait Union[-F, +B, C] extends Schema[F, B, C], Union.Reader[F, B, C], Union.Writer[F, B, C]:
  override def imap[D](f: C => D)(g: D => C): Union[F, B, D] = Union.Transform(this, f, g)
  override def optional: Union[F, B, Option[C]] = Union.Optional(this)
  def orElseWith[F1 <: F, D, E](merge: (Metadata, Metadata) => Metadata)(
      union: Union[F1, D, E]
  ): Union[F1, B | D, Either[C, E]] = Union.Combine(merge(metadata, union.metadata), this, union)
  override def update(f: Metadata => Metadata): Union[F, B, C]

object Union:
  sealed trait Value[-F, +B, C]
      extends Base.Value[F, B, C],
        Union[F, B, C],
        Union.Value.Reader[F, B, C],
        Union.Value.Writer[F, B, C]:
    override def imap[D](f: C => D)(g: D => C): Union.Value[F, B, D] = Value.Transform(this, f, g)
    final override def optional: Union.Value[F, B, Option[C]] = Value.Optional(this)
    def orElseWith[F1 <: F, D, E](merge: (Metadata, Metadata) => Metadata)(
        union: Union.Value[F1, D, E]
    ): Union.Value[F1, B | D, Either[C, E]] = Value.Combine(merge(metadata, union.metadata), this, union)
    override def update(f: Metadata => Metadata): Union.Value[F, B, C]

  object Value:
    sealed trait Required[-F, +B, C]
        extends Base.Value.Required[F, B, C],
          Union.Value[F, B, C],
          Union.Value.Required.Reader[F, B, C],
          Union.Value.Required.Writer[F, B, C]:
      override def imap[D](f: C => D)(g: D => C): Union.Value.Required[F, B, D] = Required.Transform(this, f, g)
      def orElseWith[F1 <: F, D, E](merge: (Metadata, Metadata) => Metadata)(
          union: Union.Value.Required[F1, D, E]
      ): Union.Value.Required[F1, B | D, Either[C, E]] = Required.Combine(merge(metadata, union.metadata), this, union)
      override def update(f: Metadata => Metadata): Union.Value.Required[F, B, C]

    object Required:
      sealed trait Reader[-F, +B, +C] extends Base.Value.Required.Reader[F, B, C], Union.Value.Reader[F, B, C]:
        override def map[D](f: C => D): Union.Value.Required.Reader[F, B, D] = Reader.Transform(this, f)
        def orElseWith[F1 <: F, D, E](merge: (Metadata, Metadata) => Metadata)(
            union: Union.Value.Required.Reader[F1, D, E]
        ): Union.Value.Required.Reader[F1, B | D, Either[C, E]] =
          Reader.Combine(merge(metadata, union.metadata), this, union)
        override def update(f: Metadata => Metadata): Union.Value.Required.Reader[F, B, C]

      object Reader:
        type Via[F, A] = Union.Value.Required.Reader[F, ?, A]

        final case class Combine[F, B, C, D, E](
            metadata: Metadata,
            left: Union.Value.Required.Reader[F, B, C],
            right: Union.Value.Required.Reader[F, D, E]
        ) extends Union.Value.Required.Reader[F, B | D, Either[C, E]]:
          override def update(f: Metadata => Metadata): Union.Value.Required.Reader[F, B | D, Either[C, E]] =
            copy(metadata = f(metadata))

        final case class Root[F, +B <: Base.Value.Required.Reader[F, ?, C], C](metadata: Metadata, schema: B)
            extends Union.Value.Required.Reader[F, B, C]:
          override def update(f: Metadata => Metadata): Union.Value.Required.Reader[F, B, C] =
            copy(metadata = f(metadata))

        final case class Transform[F, B, C, D](self: Union.Value.Required.Reader[F, B, C], f: C => D)
            extends Union.Value.Required.Reader[F, B, D]:
          export self.metadata
          override def update(f: Metadata => Metadata): Union.Value.Required.Reader[F, B, D] =
            copy(self = self.update(f))

      sealed trait Writer[-F, +B, -C] extends Base.Value.Required.Writer[F, B, C], Union.Value.Writer[F, B, C]:
        override def contramap[D](f: D => C): Union.Value.Required.Writer[F, B, D] = Writer.Transform(this, f)
        def orElseWith[F1 <: F, D, E](merge: (Metadata, Metadata) => Metadata)(
            union: Union.Value.Required.Writer[F1, D, E]
        ): Union.Value.Required.Writer[F1, B | D, Either[C, E]] =
          Writer.Combine(merge(metadata, union.metadata), this, union)
        override def update(f: Metadata => Metadata): Union.Value.Required.Writer[F, B, C]

      object Writer:
        type Via[F, A] = Union.Value.Required.Writer[F, ?, A]

        final case class Combine[F, B, C, D, E](
            metadata: Metadata,
            left: Union.Value.Required.Writer[F, B, C],
            right: Union.Value.Required.Writer[F, D, E]
        ) extends Union.Value.Required.Writer[F, B | D, Either[C, E]]:
          override def update(f: Metadata => Metadata): Union.Value.Required.Writer[F, B | D, Either[C, E]] =
            copy(metadata = f(metadata))

        final case class Root[F, +B <: Base.Value.Required.Writer[F, ?, C], C](metadata: Metadata, schema: B)
            extends Union.Value.Required.Writer[F, B, C]:
          override def update(f: Metadata => Metadata): Union.Value.Required.Writer[F, B, C] =
            copy(metadata = f(metadata))

        final case class Transform[F, B, C, D](self: Union.Value.Required.Writer[F, B, C], f: D => C)
            extends Union.Value.Required.Writer[F, B, D]:
          export self.metadata
          override def update(f: Metadata => Metadata): Union.Value.Required.Writer[F, B, D] =
            copy(self = self.update(f))

      final case class Combine[F, B, C, D, E](
          metadata: Metadata,
          left: Union.Value.Required[F, B, C],
          right: Union.Value.Required[F, D, E]
      ) extends Union.Value.Required[F, B | D, Either[C, E]]:
        override def update(f: Metadata => Metadata): Union.Value.Required[F, B | D, Either[C, E]] =
          copy(metadata = f(metadata))

      final case class Transform[F, B, C, D](self: Union.Value.Required[F, B, C], f: C => D, g: D => C)
          extends Union.Value.Required[F, B, D]:
        export self.metadata
        override def update(f: Metadata => Metadata): Union.Value.Required[F, B, D] = copy(self = self.update(f))

    sealed trait Reader[-F, +B, +C] extends Base.Value.Reader[F, B, C], Union.Reader[F, B, C]:
      override def map[D](f: C => D): Union.Value.Reader[F, B, D] = Reader.Transform(this, f)
      override def optional: Union.Value.Reader[F, B, Option[C]] = Reader.Optional(this)
      def orElseWith[F1 <: F, D, E](merge: (Metadata, Metadata) => Metadata)(
          union: Union.Value.Reader[F1, D, E]
      ): Union.Value.Reader[F1, B | D, Either[C, E]] = Reader.Combine(merge(metadata, union.metadata), this, union)
      override def update(f: Metadata => Metadata): Union.Value.Reader[F, B, C]

    object Reader:
      final case class Combine[F, B, C, D, E](
          metadata: Metadata,
          left: Union.Value.Reader[F, B, C],
          right: Union.Value.Reader[F, D, E]
      ) extends Union.Value.Reader[F, B | D, Either[C, E]]:
        override def update(f: Metadata => Metadata): Union.Value.Reader[F, B | D, Either[C, E]] =
          copy(metadata = f(metadata))

      final case class Optional[F, B, C, D](self: Union.Value.Reader[F, B, C])
          extends Union.Value.Reader[F, B, Option[C]]:
        export self.metadata
        override def update(f: Metadata => Metadata): Union.Value.Reader[F, B, Option[C]] = copy(self = self.update(f))

      final case class Transform[F, B, C, D](self: Union.Value.Reader[F, B, C], f: C => D)
          extends Union.Value.Reader[F, B, D]:
        export self.metadata
        override def update(f: Metadata => Metadata): Union.Value.Reader[F, B, D] = copy(self = self.update(f))

    sealed trait Writer[-F, +B, -C] extends Base.Value.Writer[F, B, C], Union.Writer[F, B, C]:
      override def contramap[D](f: D => C): Union.Value.Writer[F, B, D] = Writer.Transform(this, f)
      override def optional: Union.Value.Writer[F, B, Option[C]] = Writer.Optional(this)
      def orElseWith[F1 <: F, D, E](merge: (Metadata, Metadata) => Metadata)(
          union: Union.Value.Writer[F1, D, E]
      ): Union.Value.Writer[F1, B | D, Either[C, E]] = Writer.Combine(merge(metadata, union.metadata), this, union)
      override def update(f: Metadata => Metadata): Union.Value.Writer[F, B, C]

    object Writer:
      type Via[F, A] = Union.Value.Writer[F, ?, A]

      final case class Combine[F, B, C, D, E](
          metadata: Metadata,
          left: Union.Value.Writer[F, B, C],
          right: Union.Value.Writer[F, D, E]
      ) extends Union.Value.Writer[F, B | D, Either[C, E]]:
        override def update(f: Metadata => Metadata): Union.Value.Writer[F, B | D, Either[C, E]] =
          copy(metadata = f(metadata))

      final case class Optional[F, B, C, D](self: Union.Value.Writer[F, B, C])
          extends Union.Value.Writer[F, B, Option[C]]:
        export self.metadata
        override def update(f: Metadata => Metadata): Union.Value.Writer[F, B, Option[C]] = copy(self = self.update(f))

      final case class Transform[F, B, C, D](self: Union.Value.Writer[F, B, C], f: D => C)
          extends Union.Value.Writer[F, B, D]:
        export self.metadata
        override def update(f: Metadata => Metadata): Union.Value.Writer[F, B, D] = copy(self = self.update(f))

    final case class Combine[F, B, C, D, E](metadata: Metadata, left: Union.Value[F, B, C], right: Union.Value[F, D, E])
        extends Union.Value[F, B | D, Either[C, E]]:
      override def update(f: Metadata => Metadata): Union.Value[F, B | D, Either[C, E]] = copy(metadata = f(metadata))

    final case class Optional[F, B, C](self: Union.Value[F, B, C]) extends Union.Value[F, B, Option[C]]:
      export self.metadata
      override def update(f: Metadata => Metadata): Union.Value[F, B, Option[C]] = copy(self = self.update(f))

    final case class Transform[F, B, C, D](self: Union.Value[F, B, C], f: C => D, g: D => C)
        extends Union.Value[F, B, D]:
      export self.metadata
      override def update(f: Metadata => Metadata): Union.Value[F, B, D] = copy(self = self.update(f))

  sealed trait Reader[-F, +B, +C] extends Schema.Reader[F, B, C]:
    override def map[D](f: C => D): Union.Reader[F, B, D] = Reader.Transform(this, f)
    override def optional: Union.Reader[F, B, Option[C]] = Reader.Optional(this)
    def orElseWith[F1 <: F, D, E](merge: (Metadata, Metadata) => Metadata)(
        union: Union.Reader[F1, D, E]
    ): Union.Reader[F1, B | D, Either[C, E]] = Reader.Combine(merge(metadata, union.metadata), this, union)
    override def update(f: Metadata => Metadata): Union.Reader[F, B, C]

  object Reader:
    final case class Combine[F, B, C, D, E](
        metadata: Metadata,
        left: Union.Reader[F, B, C],
        right: Union.Reader[F, D, E]
    ) extends Union.Reader[F, B | D, Either[C, E]]:
      override def update(f: Metadata => Metadata): Union.Reader[F, B | D, Either[C, E]] = copy(metadata = f(metadata))

    final case class Optional[F, B, C](self: Union.Reader[F, B, C]) extends Union.Reader[F, B, Option[C]]:
      export self.metadata
      override def update(f: Metadata => Metadata): Union.Reader[F, B, Option[C]] = copy(self = self.update(f))

    final case class Root[F, +B <: Schema.Reader[F, ?, C], C](metadata: Metadata, schema: B)
        extends Union.Reader[F, B, C]:
      override def update(f: Metadata => Metadata): Union.Reader[F, B, C] = copy(metadata = f(metadata))

    final case class Transform[F, B, C, D](self: Union.Reader[F, B, C], f: C => D) extends Union.Reader[F, B, D]:
      export self.metadata
      override def update(f: Metadata => Metadata): Union.Reader[F, B, D] = copy(self = self.update(f))

  sealed trait Writer[-F, +B, -C] extends Schema.Writer[F, B, C]:
    override def contramap[D](f: D => C): Union.Writer[F, B, D] = Writer.Transform(this, f)
    override def optional: Union.Writer[F, B, Option[C]] = Writer.Optional(this)
    def orElseWith[F1 <: F, D, E](merge: (Metadata, Metadata) => Metadata)(
        union: Union.Writer[F1, D, E]
    ): Union.Writer[F1, B | D, Either[C, E]] = Writer.Combine(merge(metadata, union.metadata), this, union)
    override def update(f: Metadata => Metadata): Union.Writer[F, B, C]

  object Writer:
    final case class Combine[F, B, C, D, E](
        metadata: Metadata,
        left: Union.Writer[F, B, C],
        right: Union.Writer[F, D, E]
    ) extends Union.Writer[F, B | D, Either[C, E]]:
      override def update(f: Metadata => Metadata): Union.Writer[F, B | D, Either[C, E]] = copy(metadata = f(metadata))

    final case class Optional[F, B, C](self: Union.Writer[F, B, C]) extends Union.Writer[F, B, Option[C]]:
      export self.metadata
      override def update(f: Metadata => Metadata): Union.Writer[F, B, Option[C]] = copy(self = self.update(f))

    final case class Root[F, +B <: Schema.Writer[F, ?, C], C](metadata: Metadata, schema: B)
        extends Union.Writer[F, B, C]:
      override def update(f: Metadata => Metadata): Union.Writer[F, B, C] = copy(metadata = f(metadata))

    final case class Transform[F, B, C, D](self: Union.Writer[F, B, C], f: D => C) extends Union.Writer[F, B, D]:
      export self.metadata
      override def update(f: Metadata => Metadata): Union.Writer[F, B, D] = copy(self = self.update(f))

  final case class Combine[F, B, C, D, E](metadata: Metadata, left: Union[F, B, C], right: Union[F, D, E])
      extends Union[F, B | D, Either[C, E]]:
    override def update(f: Metadata => Metadata): Union[F, B | D, Either[C, E]] = copy(metadata = f(metadata))

  final case class Optional[F, B, C](self: Union[F, B, C]) extends Union[F, B, Option[C]]:
    export self.metadata
    override def update(f: Metadata => Metadata): Union[F, B, Option[C]] = copy(self = self.update(f))

  final case class Root[F, +B <: Schema[F, ?, C], C](metadata: Metadata, schema: B) extends Union[F, B, C]:
    override def update(f: Metadata => Metadata): Union[F, B, C] = copy(metadata = f(metadata))

  final case class Transform[F, B, C, D](self: Union[F, B, C], f: C => D, g: D => C) extends Union[F, B, D]:
    export self.metadata
    override def update(f: Metadata => Metadata): Union[F, B, D] = copy(self = self.update(f))
