package io.taig.otter

import cats.syntax.all.*
import cats.data.Chain
import cats.data.NonEmptyChain
import io.taig.otter.validation.Validation
import cats.Functor
import io.taig.otter as Base
import scala.Product as SProduct
import io.taig.otter
import cats.Eq
import cats.Id as Identity
import io.taig.enumeration.ext.Mapping
import scala.reflect.ClassTag

sealed trait Schema[-A, +B, C] extends Schema.Reader[A, B, C], Schema.Writer[A, B, C]:
  def imap[D](f: C => D)(g: D => C): Schema[A, B, D]
  override def optional: Schema[A, B, Option[C]]

object Schema:
  sealed trait Reader[-A, +B, +C] extends SProduct, Serializable:
    def map[D](f: C => D): Schema.Reader[A, B, D]
    def optional: Schema.Reader[A, B, Option[C]]

  sealed trait Writer[-A, +B, -C] extends SProduct, Serializable:
    def contramap[D](f: D => C): Schema.Writer[A, B, D]
    def optional: Schema.Writer[A, B, Option[C]]

sealed trait Value[-A, +B, C] extends Schema[A, B, C], Value.Reader[A, B, C], Value.Writer[A, B, C]:
  override def imap[D](f: C => D)(g: D => C): Value[A, B, D]
  override def optional: Value[A, B, Option[C]]

object Value:
  sealed trait Required[-A, +B, C]
      extends Value[A, B, C],
        Value.Required.Reader[A, B, C],
        Value.Required.Writer[A, B, C]:
    override def imap[D](f: C => D)(g: D => C): Value.Required[A, B, D]

  object Required:
    sealed trait Reader[-A, +B, +C] extends Value.Reader[A, B, C]:
      override def map[D](f: C => D): Value.Required.Reader[A, B, D]

    sealed trait Writer[-A, +B, -C] extends Value.Writer[A, B, C]:
      override def contramap[D](f: D => C): Value.Required.Writer[A, B, D]

  sealed trait Reader[-A, +B, +C] extends Schema.Reader[A, B, C]:
    override def map[D](f: C => D): Value.Reader[A, B, D]
    override def optional: Value.Reader[A, B, Option[C]]

  sealed trait Writer[-A, +B, -C] extends Schema.Writer[A, B, C]:
    override def contramap[D](f: D => C): Value.Writer[A, B, D]
    override def optional: Value.Writer[A, B, Option[C]]

sealed trait Collection[-A, +B, C] extends Schema[A, B, C], Collection.Reader[A, B, C], Collection.Writer[A, B, C]:
  final override def imap[D](f: C => D)(g: D => C): Collection[A, B, D] = ivalidate(Validation.lift(f))(g)
  final def ivalidate[D, E](validation: SchemaValidation.Collection[C, D, E])(f: E => C): Collection[A, B, E] =
    Collection.Transform(this, validation, f)
  final override def optional: Collection[A, B, Option[C]] = Collection.Optional(this)
  override def schema: Schema[A, ?, ?]

object Collection:
  sealed trait Reader[-A, +B, +C] extends Schema.Reader[A, B, C]:
    def constraints: Chain[Constraint.Collection]
    final override def map[D](f: C => D): Collection.Reader[A, B, D] = validate(Validation.lift(f))
    override def optional: Collection.Reader[A, B, Option[C]] = Reader.Optional(this)
    def schema: Schema.Reader[?, ?, ?]
    final def validate[C1 >: C, D, E](
        validation: SchemaValidation.Collection[C1, D, E]
    ): Collection.Reader[A, B, E] = Reader.Transform(this, validation)

  object Reader:
    final case class Optional[A, B, C](self: Collection.Reader[A, B, C]) extends Collection.Reader[A, B, Option[C]]:
      export self.{constraints, schema}

    final case class Root[A, +B <: Schema.Reader[A, ?, C], C](schema: B) extends Collection.Reader[A, B, Vector[C]]:
      override def constraints: Chain[Constraint.Collection] = Chain.empty

    final case class Transform[A, B, C, D, E](
        self: Collection.Reader[A, B, C],
        validation: SchemaValidation.Collection[C, D, E]
    ) extends Collection.Reader[A, B, E]:
      export self.schema
      override def constraints: Chain[Constraint.Collection] = self.constraints ++ validation.constraints

  sealed trait Writer[-A, +B, -C] extends Schema.Writer[A, B, C]:
    final def contramap[D](f: D => C): Collection.Writer[A, B, D] = Writer.Transform(this, f)
    def optional: Collection.Writer[A, B, Option[C]] = Writer.Optional(this)
    def schema: Schema.Writer[A, ?, ?]

  object Writer:
    final case class Transform[A, B, C, D](
        self: Collection.Writer[A, B, C],
        f: D => C
    ) extends Collection.Writer[A, B, D]:
      export self.schema

    final case class Optional[A, B, C](self: Collection.Writer[A, B, C]) extends Collection.Writer[A, B, Option[C]]:
      export self.schema

    final case class Root[A, +B <: Schema.Writer[A, ?, C], C](schema: B) extends Collection.Writer[A, B, Vector[C]]

  final case class Optional[A, B, C](self: Collection[A, B, C]) extends Collection[A, B, Option[C]]:
    export self.{constraints, schema}

  final case class Root[A, +B <: Schema[A, ?, C], C](schema: B) extends Collection[A, B, Vector[C]]:
    override def constraints: Chain[Constraint.Collection] = Chain.empty

  final case class Transform[A, B, C, D, E](
      self: Collection[A, B, C],
      validation: SchemaValidation.Collection[C, D, E],
      f: E => C
  ) extends Collection[A, B, E]:
    export self.schema
    override def constraints: Chain[Constraint.Collection] = self.constraints ++ validation.constraints

sealed trait Dictionary[-A, +B, C] extends Schema[A, B, C], Dictionary.Reader[A, B, C], Dictionary.Writer[A, B, C]:
  final override def imap[D](f: C => D)(g: D => C): Dictionary[A, B, D] = Dictionary.Transform(this, f, g)
  final override def optional: Dictionary[A, B, Option[C]] = Dictionary.Optional(this)

object Dictionary:
  sealed trait Reader[-A, +B, +C] extends Schema.Reader[A, B, C]:
    final override def map[D](f: C => D): Dictionary.Reader[A, B, D] = Reader.Transform(this, f)
    override def optional: Dictionary.Reader[A, B, Option[C]] = Reader.Optional(this)

  object Reader:
    final case class Optional[A, B, C](self: Dictionary.Reader[A, B, C]) extends Dictionary.Reader[A, B, Option[C]]

    final case class Root[A, B, +C <: Schema.Reader[A, ?, D], D](
        key: Primitive.Required.Reader[B],
        value: C
    ) extends Dictionary.Reader[A, C, List[(B, D)]]

    final case class Transform[A, B, C, D](self: Dictionary.Reader[A, B, C], f: C => D)
        extends Dictionary.Reader[A, B, D]

  sealed trait Writer[-A, +B, -C] extends Schema.Writer[A, B, C]:
    override def contramap[D](f: D => C): Dictionary.Writer[A, B, D] = Writer.Transform(this, f)
    override def optional: Dictionary.Writer[A, B, Option[C]] = Writer.Optional(this)

  object Writer:
    final case class Optional[A, B, C](self: Dictionary.Writer[A, B, C]) extends Dictionary.Writer[A, B, Option[C]]

    final case class Root[A, B, +C <: Schema.Writer[A, ?, D], D](
        key: Primitive.Required.Writer[B],
        value: C
    ) extends Dictionary.Writer[A, C, List[(B, D)]]

    final case class Transform[A, B, C, D](self: Dictionary.Writer[A, B, C], f: D => C)
        extends Dictionary.Writer[A, B, D]

  final case class Optional[A, B, C](self: Dictionary[A, B, C]) extends Dictionary[A, B, Option[C]]

  final case class Root[A, B, +C <: Schema[A, ?, D], D](key: Primitive.Required[B], value: C)
      extends Dictionary[A, C, List[(B, D)]]

  final case class Transform[A, B, C, D](self: Dictionary[A, B, C], f: C => D, g: D => C) extends Dictionary[A, B, D]

sealed trait Dynamic[A, B] extends Schema[A, Nothing, B], Dynamic.Reader[A, B], Dynamic.Writer[A, B]:
  override def optional: Dynamic[A, Option[B]] = Dynamic.Optional(this)
  override def imap[C](f: B => C)(g: C => B): Dynamic[A, C] = Dynamic.Transform(this, f, g)

object Dynamic:
  sealed trait Reader[-A, +B] extends Schema.Reader[A, Nothing, B]:
    override def optional: Dynamic.Reader[A, Option[B]] = Reader.Optional(this)
    final override def map[C](f: B => C): Dynamic.Reader[A, C] = Reader.Transform(this, f)

  object Reader:
    final case class Optional[A, B](self: Dynamic.Reader[A, B]) extends Dynamic.Reader[A, Option[B]]

    final case class Root[A]() extends Dynamic.Reader[A, A]

    final case class Transform[A, B, C](self: Dynamic.Reader[A, B], f: B => C) extends Dynamic.Reader[A, C]

  sealed trait Writer[A, -B] extends Schema.Writer[A, Nothing, B]:
    override def optional: Dynamic.Writer[A, Option[B]] = Writer.Optional(this)
    final override def contramap[C](f: C => B): Dynamic.Writer[A, C] = Writer.Transform(this, f)

  object Writer:
    final case class Optional[A, B](self: Dynamic.Writer[A, B]) extends Dynamic.Writer[A, Option[B]]

    final case class Root[A]() extends Dynamic.Writer[A, A]

    final case class Transform[A, B, C](self: Dynamic.Writer[A, B], f: C => B) extends Dynamic.Writer[A, C]

  final case class Optional[A, B](self: Dynamic[A, B]) extends Dynamic[A, Option[B]]

  final case class Root[A]() extends Dynamic[A, A]

  final case class Transform[A, B, C](self: Dynamic[A, B], f: B => C, g: C => B) extends Dynamic[A, C]

sealed trait Enumeration[-A, +B, C] extends Value[A, B, C], Enumeration.Reader[A, B, C], Enumeration.Writer[A, B, C]:
  override def imap[D](f: C => D)(g: D => C): Enumeration[A, B, D] = Enumeration.Transform(this, f, g)
  override def optional: Enumeration[A, B, Option[C]] = Enumeration.Optional(this)

object Enumeration:
  sealed trait Required[-A, +B, C]
      extends Value.Required[A, B, C],
        Enumeration[A, B, C],
        Enumeration.Required.Reader[A, B, C],
        Enumeration.Required.Writer[A, B, C]:
    override def imap[D](f: C => D)(g: D => C): Enumeration.Required[A, B, D] = Required.Transform(this, f, g)

  object Required:
    sealed trait Reader[-A, +B, +C] extends Value.Required.Reader[A, B, C], Enumeration.Reader[A, B, C]:
      override def map[D](f: C => D): Enumeration.Required.Reader[A, B, D] = Reader.Transform(this, f)

    object Reader:
      final case class Root[A, +B <: Value.Required.Reader[A, ?, C], C, D](
          schema: B,
          mapping: Mapping[D, C],
          writer: Schema.Writer[A, ?, C]
      ) extends Enumeration.Required.Reader[A, B, D]

      final case class Transform[A, B, C, D](self: Enumeration.Required.Reader[A, B, C], f: C => D)
          extends Enumeration.Required.Reader[A, B, D]

    sealed trait Writer[-A, +B, -C] extends Value.Required.Writer[A, B, C], Enumeration.Writer[A, B, C]:
      override def contramap[D](f: D => C): Enumeration.Required.Writer[A, B, D] = Writer.Transform(this, f)

    object Writer:
      final case class Root[A, +B <: Value.Required.Writer[A, ?, C], C, D](schema: B, f: D => C)
          extends Enumeration.Required.Writer[A, B, D]

      final case class Transform[A, B, C, D](self: Enumeration.Required.Writer[A, B, C], f: D => C)
          extends Enumeration.Required.Writer[A, B, D]

    final case class Transform[A, B, C, D](self: Enumeration.Required[A, B, C], f: C => D, g: D => C)
        extends Enumeration.Required[A, B, D]

  sealed trait Reader[-A, +B, +C] extends Value.Reader[A, B, C]:
    override def map[D](f: C => D): Enumeration.Reader[A, B, D] = Reader.Transform(this, f)
    override def optional: Enumeration.Reader[A, B, Option[C]] = Reader.Optional(this)

  object Reader:
    final case class Optional[A, B, C](self: Enumeration.Reader[A, B, C]) extends Enumeration.Reader[A, B, Option[C]]

    final case class Root[A, +B <: Value.Reader[A, ?, C], C, D](
        schema: B,
        mapping: Mapping[D, C],
        writer: Schema.Writer[A, ?, C]
    ) extends Enumeration.Reader[A, B, D]

    final case class Transform[A, B, C, D](self: Enumeration.Reader[A, B, C], f: C => D)
        extends Enumeration.Reader[A, B, D]

  sealed trait Writer[-A, +B, -C] extends Value.Writer[A, B, C]:
    override def contramap[D](f: D => C): Enumeration.Writer[A, B, D] = Writer.Transform(this, f)
    override def optional: Enumeration.Writer[A, B, Option[C]] = Writer.Optional(this)

  object Writer:
    final case class Optional[A, B, C](self: Enumeration.Writer[A, B, C]) extends Enumeration.Writer[A, B, Option[C]]

    final case class Transform[A, B, C, D](self: Enumeration.Writer[A, B, C], f: D => C)
        extends Enumeration.Writer[A, B, D]

  final case class Optional[A, B, C](self: Enumeration[A, B, C]) extends Enumeration[A, B, Option[C]]

  final case class Root[A, +B <: Value[A, ?, C], C, D](schema: B, mapping: Mapping[D, C]) extends Enumeration[A, B, D]

  final case class Transform[A, B, C, D](self: Enumeration[A, B, C], f: C => D, g: D => C) extends Enumeration[A, B, D]

sealed trait Primitive[A] extends Value[Any, Nothing, A], Primitive.Reader[A], Primitive.Writer[A]:
  override def imap[B](f: A => B)(g: B => A): Primitive[B] = ivalidate(Validation.lift(f))(g)
  final override def optional: Primitive[Option[A]] = Primitive.Optional(this)
  def ivalidate[B, C, D](validation: SchemaValidation.Primitive[A, B, C, D])(
      f: D => A
  ): Primitive[D] = Primitive.Transform(this, validation, f)

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

  object Required:
    sealed trait Reader[+A] extends Value.Required.Reader[Any, Nothing, A], Primitive.Reader[A]:
      final override def map[C](f: A => C): Primitive.Required.Reader[C] = validate(Validation.lift(f))
      final override def validate[A1 >: A, B, C, D](
          transformation: SchemaValidation.Primitive[A1, B, C, D]
      ): Primitive.Required.Reader[D] = Reader.Transform(this, transformation)

    object Reader:
      final case class Transform[A, B, C, D](
          self: Primitive.Required.Reader[A],
          validation: SchemaValidation.Primitive[A, B, C, D]
      ) extends Primitive.Required.Reader[D]:
        export self.tpe
        override def constraints: Chain[Constraint.Primitive[?]] = self.constraints ++ validation.constraints

    sealed trait Writer[-A] extends Value.Required.Writer[Any, Nothing, A], Primitive.Writer[A]:
      final override def contramap[B](f: B => A): Primitive.Required.Writer[B] = Writer.Transform(this, f)

    object Writer:
      final case class Transform[A, B](self: Primitive.Required.Writer[A], f: B => A)
          extends Primitive.Required.Writer[B]:
        export self.tpe

    final case class Root[A](tpe: Type[A]) extends Primitive.Required[A]:
      override def constraints: Chain[Constraint.Primitive[?]] = Chain.empty

    final case class Transform[A, B, C, D](
        self: Primitive.Required[A],
        validation: SchemaValidation.Primitive[A, B, C, D],
        f: D => A
    ) extends Primitive.Required[D]:
      export self.tpe
      override def constraints: Chain[Constraint.Primitive[?]] = self.constraints ++ validation.constraints

  sealed trait Reader[+A] extends Value.Reader[Any, Nothing, A]:
    def constraints: Chain[Constraint.Primitive[?]]
    override def map[C](f: A => C): Primitive.Reader[C] = validate(Validation.lift(f))
    override def optional: Primitive.Reader[Option[A]] = Reader.Optional(this)
    def tpe: Type[?]
    def validate[A1 >: A, B, C, D](
        validation: SchemaValidation.Primitive[A1, B, C, D]
    ): Primitive.Reader[D] = Reader.Transform(this, validation)

  object Reader:
    final case class Transform[A, B, C, D](
        self: Primitive.Reader[A],
        validation: SchemaValidation.Primitive[A, B, C, D]
    ) extends Primitive.Reader[D]:
      export self.tpe
      override def constraints: Chain[Constraint.Primitive[?]] = self.constraints ++ validation.constraints

    final case class Optional[A](self: Primitive.Reader[A]) extends Primitive.Reader[Option[A]]:
      export self.{constraints, tpe}

  sealed trait Writer[-A] extends Value.Writer[Any, Nothing, A]:
    override def contramap[B](f: B => A): Primitive.Writer[B] = Writer.Transform(this, f)
    override def optional: Primitive.Writer[Option[A]] = Writer.Optional(this)
    def tpe: Type[?]

  object Writer:
    final case class Transform[A, B](self: Primitive.Writer[A], f: B => A) extends Primitive.Writer[B]:
      export self.tpe

    final case class Optional[A](self: Primitive.Writer[A]) extends Primitive.Writer[Option[A]]:
      export self.tpe

  final case class Optional[A](self: Primitive[A]) extends Primitive[Option[A]]:
    export self.{constraints, tpe}

  final case class Transform[A, B, C, D](
      self: Primitive[A],
      validation: SchemaValidation.Primitive[A, B, C, D],
      f: D => A
  ) extends Primitive[D]:
    export self.tpe
    override def constraints: Chain[Constraint.Primitive[?]] = self.constraints ++ validation.constraints

sealed trait Product[-A, +B, C] extends Schema[A, B, C], Product.Reader[A, B, C], Product.Writer[A, B, C]:
  override def imap[D](f: C => D)(g: D => C): Product[A, B, D] = Product.Transform(this, f, g)
  override def optional: Product[A, B, Option[C]] = Product.Optional(this)
  def product[A1 <: A, D, E](product: Product[A1, D, E]): Product[A1, B & D, (C, E)] =
    Product.Combine(this, product)
  def schemas: Chain[Schema[A, ?, ?]]

object Product:
  sealed trait Reader[-A, +B, +C] extends Schema.Reader[A, B, C]:
    override def map[D](f: C => D): Product.Reader[A, B, D] = Reader.Transform(this, f)
    override def optional: Product.Reader[A, B, Option[C]] = Reader.Optional(this)
    def product[A1 <: A, D, E](product: Product.Reader[A1, D, E]): Product.Reader[A1, B & D, (C, E)] =
      Reader.Combine(this, product)
    def schemas: Chain[Schema.Reader[A, ?, ?]]

  object Reader:
    final case class Combine[A, B, C, D, E](left: Product.Reader[A, B, C], right: Product.Reader[A, D, E])
        extends Product.Reader[A, B & D, (C, E)]:
      override def schemas: Chain[Schema.Reader[A, ?, ?]] = left.schemas ++ right.schemas

    final case class One[A, +B <: Schema.Reader[A, ?, C], C](schema: B) extends Product.Reader[A, B, C]:
      override def schemas: Chain[Schema.Reader[A, ?, ?]] = Chain.one(schema)

    final case class Optional[A, B, C](self: Product.Reader[A, B, C]) extends Product.Reader[A, B, Option[C]]:
      export self.schemas

    final case class Transform[A, B, C, D](self: Product.Reader[A, B, C], f: C => D) extends Product.Reader[A, B, D]:
      export self.schemas

  sealed trait Writer[-A, +B, -C] extends Schema.Writer[A, B, C]:
    override def contramap[D](f: D => C): Product.Writer[A, B, D] = Writer.Transform(this, f)
    override def optional: Product.Writer[A, B, Option[C]] = Writer.Optional(this)
    def product[A1 <: A, D, E](product: Product.Writer[A1, D, E]): Product.Writer[A1, B & D, (C, E)] =
      Writer.Combine(this, product)
    def schemas: Chain[Schema.Writer[A, ?, ?]]

  object Writer:
    final case class Combine[A, B, C, D, E](left: Product.Writer[A, B, C], right: Product.Writer[A, D, E])
        extends Product.Writer[A, B & D, (C, E)]:
      override def schemas: Chain[Schema.Writer[A, ?, ?]] = left.schemas ++ right.schemas

    final case class One[A, +B <: Schema.Writer[A, ?, C], C](schema: B) extends Product.Writer[A, B, C]:
      override def schemas: Chain[Schema.Writer[A, ?, ?]] = Chain.one(schema)

    final case class Optional[A, B, C](self: Product.Writer[A, B, C]) extends Product.Writer[A, B, Option[C]]:
      export self.schemas

    final case class Transform[A, B, C, D](self: Product.Writer[A, B, C], f: D => C) extends Product.Writer[A, B, D]:
      export self.schemas

  final case class Combine[A, B, C, D, E](left: Product[A, B, C], right: Product[A, D, E])
      extends Product[A, B & D, (C, E)]:
    override def schemas: Chain[Schema[A, ?, ?]] = left.schemas ++ right.schemas

  case object Empty extends Product[Any, Nothing, Unit]:
    override def schemas: Chain[Nothing] = Chain.empty

  final case class One[A, +B <: Schema[A, ?, C], C](schema: B) extends Product[A, B, C]:
    override def schemas: Chain[Schema[A, ?, ?]] = Chain.one(schema)

  final case class Optional[A, B, C](self: Product[A, B, C]) extends Product[A, B, Option[C]]:
    export self.schemas

  final case class Transform[A, B, C, D](self: Product[A, B, C], f: C => D, g: D => C) extends Product[A, B, D]:
    export self.schemas

sealed trait Record[-A, +B, C] extends Schema[A, B, C], Record.Reader[A, B, C], Record.Writer[A, B, C]:
  override def nulls: Record.Null
  final def nulls(value: Record.Null): Record.Writer[A, B, C] = Record.Nulls(this, value)

  def fields: Chain[Field[A, ?, ?]]
  override def imap[D](f: C => D)(g: D => C): Record[A, B, D] = Record.Transform(this, f, g)
  override def optional: Record[A, B, Option[C]] = Record.Optional(this)
  def product[A1 <: A, D, E](product: Record[A1, D, E]): Record[A1, B & D, (C, E)] =
    Record.Combine(this, product)

object Record:
  sealed trait Reader[-A, +B, +C] extends Schema.Reader[A, B, C]:
    def fields: Chain[Field.Reader[A, ?, ?]]
    override def map[D](f: C => D): Record.Reader[A, B, D] = Reader.Transform(this, f)
    override def optional: Record.Reader[A, B, Option[C]] = Reader.Optional(this)
    def product[A1 <: A, D, E](product: Record.Reader[A1, D, E]): Record.Reader[A1, B & D, (C, E)] =
      Reader.Combine(this, product)

  object Reader:
    final case class Combine[A, B, C, D, E](left: Record.Reader[A, B, C], right: Record.Reader[A, D, E])
        extends Record.Reader[A, B & D, (C, E)]:
      override def fields: Chain[Field.Reader[A, ?, ?]] = left.fields ++ right.fields

    final case class One[A, B, C](field: Field.Reader[A, B, C]) extends Record.Reader[A, B, C]:
      override def fields: Chain[Field.Reader[A, ?, ?]] = Chain.one(field)

    final case class Optional[A, B, C](self: Record.Reader[A, B, C]) extends Record.Reader[A, B, Option[C]]:
      export self.fields

    final case class Transform[A, B, C, D](self: Record.Reader[A, B, C], f: C => D) extends Record.Reader[A, B, D]:
      export self.fields

  sealed trait Writer[-A, +B, -C] extends Schema.Writer[A, B, C]:
    def nulls: Record.Null

    override def contramap[D](f: D => C): Record.Writer[A, B, D] = Writer.Transform(this, f)
    def fields: Chain[Field.Writer[A, ?, ?]]
    override def optional: Record.Writer[A, B, Option[C]] = Writer.Optional(this)
    def product[A1 <: A, D, E](product: Record.Writer[A1, D, E]): Record.Writer[A1, B & D, (C, E)] =
      Writer.Combine(this, product)

  object Writer:
    final case class Combine[A, B, C, D, E](left: Record.Writer[A, B, C], right: Record.Writer[A, D, E])
        extends Record.Writer[A, B & D, (C, E)]:
      override def fields: Chain[Field.Writer[A, ?, ?]] = left.fields ++ right.fields
      override def nulls: Record.Null = Record.Null.Default

    final case class Nulls[A, B, C](self: Record.Writer[A, B, C], nulls: Record.Null) extends Record.Writer[A, B, C]:
      export self.fields

    final case class One[A, B, C](field: Field.Writer[A, B, C]) extends Record.Writer[A, B, C]:
      override def nulls: Record.Null = Record.Null.Default
      override def fields: Chain[Field.Writer[A, ?, ?]] = Chain.one(field)

    final case class Optional[A, B, C](self: Record.Writer[A, B, C]) extends Record.Writer[A, B, Option[C]]:
      export self.{fields, nulls}

    final case class Transform[A, B, C, D](self: Record.Writer[A, B, C], f: D => C) extends Record.Writer[A, B, D]:
      export self.{fields, nulls}

  case object Empty extends Record[Any, Nothing, Unit]:
    override def nulls: Record.Null = Record.Null.Default
    override def fields: Chain[Nothing] = Chain.empty

  final case class Combine[A, B, C, D, E](left: Record[A, B, C], right: Record[A, D, E])
      extends Record[A, B & D, (C, E)]:
    override def nulls: Record.Null = Record.Null.Default
    override def fields: Chain[Field[A, ?, ?]] = left.fields ++ right.fields

  final case class Nulls[A, B, C](self: Record[A, B, C], nulls: Record.Null) extends Record[A, B, C]:
    export self.fields

  final case class One[A, B, C](field: Field[A, B, C]) extends Record[A, B, C]:
    override def nulls: Record.Null = Record.Null.Default
    override def fields: Chain[Field[A, ?, ?]] = Chain.one(field)

  final case class Optional[A, B, C](self: Record[A, B, C]) extends Record[A, B, Option[C]]:
    export self.{fields, nulls}

  final case class Transform[A, B, C, D](self: Record[A, B, C], f: C => D, g: D => C) extends Record[A, B, D]:
    export self.{fields, nulls}

  enum Null:
    case Show
    case Hide

  object Null:
    val Default: Null = Show
    given Eq[Null] = Eq.fromUniversalEquals

sealed trait Sum[-A, +B, C] extends Schema[A, B, C], Sum.Reader[A, B, C], Sum.Writer[A, B, C]:
  final override def discriminator(value: Sum.Discriminator): Sum[A, B, C] = Sum.Discriminators(this, value)

  override def branches: NonEmptyChain[Branch[A, ?, ?]]
  override def imap[D](f: C => D)(g: D => C): Sum[A, B, D] = Sum.Transform(this, f, g)
  override def optional: Sum[A, B, Option[C]] = Sum.Optional(this)
  def orElse[A1 <: A, D, E](sum: Sum[A1, D, E]): Sum[A1, B | D, Either[C, E]] = Sum.Combine(this, sum)

object Sum:
  sealed trait Reader[-A, +B, +C] extends Schema.Reader[A, B, C]:
    def discriminator: Sum.Discriminator
    def discriminator(value: Sum.Discriminator): Sum.Reader[A, B, C] = Reader.Discriminators(this, value)

    def branches: NonEmptyChain[Branch.Reader[A, ?, ?]]
    final override def map[D](f: C => D): Sum.Reader[A, B, D] = Reader.Transform(this, f)
    override def optional: Sum.Reader[A, B, Option[C]] = Reader.Optional(this)
    def orElse[A1 <: A, D, E](sum: Sum.Reader[A1, D, E]): Sum.Reader[A1, B | D, Either[C, E]] =
      Reader.Combine(this, sum)

  object Reader:
    final case class Combine[A, B, C, D, E](left: Sum.Reader[A, B, C], right: Sum.Reader[A, D, E])
        extends Sum.Reader[A, B | D, Either[C, E]]:
      override def branches: NonEmptyChain[Branch.Reader[A, ?, ?]] = left.branches ++ right.branches
      override def discriminator: Discriminator = Discriminator.Default

    final case class Discriminators[A, B, C](self: Sum.Reader[A, B, C], discriminator: Sum.Discriminator)
        extends Sum.Reader[A, B, C]:
      export self.branches
      override def discriminator(value: Sum.Discriminator): Sum.Reader[A, B, C] = ???

    final case class Optional[A, B, C](self: Sum.Reader[A, B, C]) extends Sum.Reader[A, B, Option[C]]:
      export self.{branches, discriminator}

    final case class Root[A, B, C](branch: Branch.Reader[A, B, C]) extends Sum.Reader[A, B, C]:
      override def branches: NonEmptyChain[Branch.Reader[A, B, C]] = NonEmptyChain.one(branch)
      override def discriminator: Discriminator = Discriminator.Default

    final case class Transform[A, B, C, D](self: Sum.Reader[A, B, C], f: C => D) extends Sum.Reader[A, B, D]:
      export self.{branches, discriminator}

  sealed trait Writer[-A, +B, -C] extends Schema.Writer[A, B, C]:
    def discriminator: Discriminator
    def discriminator(value: Discriminator): Sum.Writer[A, B, C] = Writer.Discriminators(this, value)

    def branches: NonEmptyChain[Branch.Writer[A, ?, ?]]
    final override def contramap[D](f: D => C): Sum.Writer[A, B, D] = Writer.Transform(this, f)
    override def optional: Sum.Writer[A, B, Option[C]] = Writer.Optional(this)
    def orElse[A1 <: A, D, E](sum: Sum.Writer[A1, D, E]): Sum.Writer[A1, B | D, Either[C, E]] =
      Writer.Combine(this, sum)

  object Writer:
    final case class Combine[A, B, C, D, E](left: Sum.Writer[A, B, C], right: Sum.Writer[A, D, E])
        extends Sum.Writer[A, B | D, Either[C, E]]:
      override def branches: NonEmptyChain[Branch.Writer[A, ?, ?]] = left.branches ++ right.branches
      override def discriminator: Sum.Discriminator = Sum.Discriminator.Default

    final case class Discriminators[A, B, C](self: Sum.Writer[A, B, C], discriminator: Sum.Discriminator)
        extends Sum.Writer[A, B, C]:
      export self.branches
      override def discriminator(value: Sum.Discriminator): Sum.Writer[A, B, C] = ???

    final case class Optional[A, B, C](self: Sum.Writer[A, B, C]) extends Sum.Writer[A, B, Option[C]]:
      export self.{branches, discriminator}

    final case class Root[A, B, C](branch: Branch.Writer[A, B, C]) extends Sum.Writer[A, B, C]:
      override def branches: NonEmptyChain[Branch.Writer[A, B, C]] = NonEmptyChain.one(branch)
      override def discriminator: Sum.Discriminator = Sum.Discriminator.Default

    final case class Transform[A, B, C, D](self: Sum.Writer[A, B, C], f: D => C) extends Sum.Writer[A, B, D]:
      export self.{branches, discriminator}

  final case class Combine[A, B, C, D, E](left: Sum[A, B, C], right: Sum[A, D, E]) extends Sum[A, B | D, Either[C, E]]:
    override def branches: NonEmptyChain[Branch[A, ?, ?]] = left.branches ++ right.branches
    override def discriminator: Sum.Discriminator = Sum.Discriminator.Default

  final case class Discriminators[A, B, C](self: Sum[A, B, C], discriminator: Sum.Discriminator) extends Sum[A, B, C]:
    export self.branches

  final case class Optional[A, B, C](self: Sum[A, B, C]) extends Sum[A, B, Option[C]]:
    export self.{branches, discriminator}

  final case class Root[A, B, C](branch: Branch[A, B, C]) extends Sum[A, B, C]:
    override def branches: NonEmptyChain[Branch[A, B, C]] = NonEmptyChain.one(branch)
    override def discriminator: Sum.Discriminator = Sum.Discriminator.Default

  final case class Transform[A, B, C, D](self: Sum[A, B, C], f: C => D, g: D => C) extends Sum[A, B, D]:
    export self.{branches, discriminator}

  enum Discriminator:
    case Nested(identifier: String, value: String)
    case Merged(identifier: String)
    case Keyed

  object Discriminator:
    object Nested:
      val Default: Discriminator.Nested = Nested(identifier = "type", value = "value")

    object Merged:
      val Default: Discriminator.Merged = Merged(identifier = "type")

    val Default: Discriminator = Nested.Default

    given Eq[Discriminator] = Eq.fromUniversalEquals

sealed trait Union[-A, +B, C] extends Schema[A, B, C], Union.Reader[A, B, C], Union.Writer[A, B, C]:
  override def imap[D](f: C => D)(g: D => C): Union[A, B, D] = Union.Transform(this, f, g)
  override def optional: Union[A, B, Option[C]] = Union.Optional(this)
  def orElse[A1 <: A, D, E](union: Union[A1, D, E]): Union[A1, B | D, Either[C, E]] =
    Union.Combine(this, union)

object Union:
  sealed trait Value[-A, +B, C]
      extends Base.Value[A, B, C],
        Union[A, B, C],
        Union.Value.Reader[A, B, C],
        Union.Value.Writer[A, B, C]:
    override def imap[D](f: C => D)(g: D => C): Union.Value[A, B, D] = Value.Transform(this, f, g)
    final override def optional: Union.Value[A, B, Option[C]] = Value.Optional(this)
    def orElse[A1 <: A, D, E](union: Union.Value[A1, D, E]): Union.Value[A1, B | D, Either[C, E]] =
      Value.Combine(this, union)

  object Value:
    sealed trait Required[-A, +B, C]
        extends Base.Value.Required[A, B, C],
          Union.Value[A, B, C],
          Union.Value.Required.Reader[A, B, C],
          Union.Value.Required.Writer[A, B, C]:
      override def imap[D](f: C => D)(g: D => C): Union.Value.Required[A, B, D] = Required.Transform(this, f, g)
      def orElse[A1 <: A, D, E](
          union: Union.Value.Required[A1, D, E]
      ): Union.Value.Required[A1, B | D, Either[C, E]] = Required.Combine(this, union)

    object Required:
      sealed trait Reader[-A, +B, +C] extends Base.Value.Required.Reader[A, B, C], Union.Value.Reader[A, B, C]:
        override def map[D](f: C => D): Union.Value.Required.Reader[A, B, D] = Reader.Transform(this, f)
        def orElse[A1 <: A, D, E](
            union: Union.Value.Required.Reader[A1, D, E]
        ): Union.Value.Required.Reader[A1, B | D, Either[C, E]] = Reader.Combine(this, union)

      object Reader:
        final case class Combine[A, B, C, D, E](
            left: Union.Value.Required.Reader[A, B, C],
            right: Union.Value.Required.Reader[A, D, E]
        ) extends Union.Value.Required.Reader[A, B | D, Either[C, E]]

        final case class Root[A, +B <: Base.Value.Required.Reader[A, ?, C], C](schema: B)
            extends Union.Value.Required.Reader[A, B, C]

        final case class Transform[A, B, C, D](self: Union.Value.Required.Reader[A, B, C], f: C => D)
            extends Union.Value.Required.Reader[A, B, D]

      sealed trait Writer[-A, +B, -C] extends Base.Value.Required.Writer[A, B, C], Union.Value.Writer[A, B, C]:
        override def contramap[D](f: D => C): Union.Value.Required.Writer[A, B, D] = Writer.Transform(this, f)
        def orElse[A1 <: A, D, E](
            union: Union.Value.Required.Writer[A1, D, E]
        ): Union.Value.Required.Writer[A1, B | D, Either[C, E]] = Writer.Combine(this, union)

      object Writer:
        final case class Combine[A, B, C, D, E](
            left: Union.Value.Required.Writer[A, B, C],
            right: Union.Value.Required.Writer[A, D, E]
        ) extends Union.Value.Required.Writer[A, B | D, Either[C, E]]

        final case class Root[A, +B <: Base.Value.Required.Writer[A, ?, C], C](schema: B)
            extends Union.Value.Required.Writer[A, B, C]

        final case class Transform[A, B, C, D](self: Union.Value.Required.Writer[A, B, C], f: D => C)
            extends Union.Value.Required.Writer[A, B, D]

      final case class Combine[A, B, C, D, E](
          left: Union.Value.Required[A, B, C],
          right: Union.Value.Required[A, D, E]
      ) extends Union.Value.Required[A, B | D, Either[C, E]]

      final case class Transform[A, B, C, D](self: Union.Value.Required[A, B, C], f: C => D, g: D => C)
          extends Union.Value.Required[A, B, D]

    sealed trait Reader[-A, +B, +C] extends Base.Value.Reader[A, B, C], Union.Reader[A, B, C]:
      override def map[D](f: C => D): Union.Value.Reader[A, B, D] = Reader.Transform(this, f)
      override def optional: Union.Value.Reader[A, B, Option[C]] = Reader.Optional(this)
      def orElse[A1 <: A, D, E](union: Union.Value.Reader[A1, D, E]): Union.Value.Reader[A1, B | D, Either[C, E]] =
        Reader.Combine(this, union)

    object Reader:
      final case class Combine[A, B, C, D, E](
          left: Union.Value.Reader[A, B, C],
          right: Union.Value.Reader[A, D, E]
      ) extends Union.Value.Reader[A, B | D, Either[C, E]]

      final case class Optional[A, B, C, D](self: Union.Value.Reader[A, B, C])
          extends Union.Value.Reader[A, B, Option[C]]

      final case class Transform[A, B, C, D](self: Union.Value.Reader[A, B, C], f: C => D)
          extends Union.Value.Reader[A, B, D]

    sealed trait Writer[-A, +B, -C] extends Base.Value.Writer[A, B, C], Union.Writer[A, B, C]:
      override def contramap[D](f: D => C): Union.Value.Writer[A, B, D] = Writer.Transform(this, f)
      override def optional: Union.Value.Writer[A, B, Option[C]] = Writer.Optional(this)
      def orElse[A1 <: A, D, E](union: Union.Value.Writer[A1, D, E]): Union.Value.Writer[A1, B | D, Either[C, E]] =
        Writer.Combine(this, union)

    object Writer:
      final case class Combine[A, B, C, D, E](
          left: Union.Value.Writer[A, B, C],
          right: Union.Value.Writer[A, D, E]
      ) extends Union.Value.Writer[A, B | D, Either[C, E]]

      final case class Optional[A, B, C, D](self: Union.Value.Writer[A, B, C])
          extends Union.Value.Writer[A, B, Option[C]]

      final case class Transform[A, B, C, D](self: Union.Value.Writer[A, B, C], f: D => C)
          extends Union.Value.Writer[A, B, D]

    final case class Combine[A, B, C, D, E](left: Union.Value[A, B, C], right: Union.Value[A, D, E])
        extends Union.Value[A, B | D, Either[C, E]]

    final case class Optional[A, B, C](self: Union.Value[A, B, C]) extends Union.Value[A, B, Option[C]]

    final case class Transform[A, B, C, D](self: Union.Value[A, B, C], f: C => D, g: D => C)
        extends Union.Value[A, B, D]

  sealed trait Reader[-A, +B, +C] extends Schema.Reader[A, B, C]:
    override def map[D](f: C => D): Union.Reader[A, B, D] = Reader.Transform(this, f)
    override def optional: Union.Reader[A, B, Option[C]] = Reader.Optional(this)
    def orElse[A1 <: A, D, E](union: Union.Reader[A1, D, E]): Union.Reader[A1, B | D, Either[C, E]] =
      Reader.Combine(this, union)

  object Reader:
    final case class Combine[A, B, C, D, E](left: Union.Reader[A, B, C], right: Union.Reader[A, D, E])
        extends Union.Reader[A, B | D, Either[C, E]]

    final case class Optional[A, B, C](self: Union.Reader[A, B, C]) extends Union.Reader[A, B, Option[C]]

    final case class Root[A, +B <: Schema.Reader[A, ?, C], C](schema: B) extends Union.Reader[A, B, C]

    final case class Transform[A, B, C, D](self: Union.Reader[A, B, C], f: C => D) extends Union.Reader[A, B, D]

  sealed trait Writer[-A, +B, -C] extends Schema.Writer[A, B, C]:
    override def contramap[D](f: D => C): Union.Writer[A, B, D] = Writer.Transform(this, f)
    override def optional: Union.Writer[A, B, Option[C]] = Writer.Optional(this)
    def orElse[A1 <: A, D, E](union: Union.Writer[A1, D, E]): Union.Writer[A1, B | D, Either[C, E]] =
      Writer.Combine(this, union)

  object Writer:
    final case class Combine[A, B, C, D, E](left: Union.Writer[A, B, C], right: Union.Writer[A, D, E])
        extends Union.Writer[A, B | D, Either[C, E]]

    final case class Optional[A, B, C](self: Union.Writer[A, B, C]) extends Union.Writer[A, B, Option[C]]

    final case class Root[A, +B <: Schema.Writer[A, ?, C], C](schema: B) extends Union.Writer[A, B, C]

    final case class Transform[A, B, C, D](self: Union.Writer[A, B, C], f: D => C) extends Union.Writer[A, B, D]

  final case class Combine[A, B, C, D, E](left: Union[A, B, C], right: Union[A, D, E])
      extends Union[A, B | D, Either[C, E]]

  final case class Optional[A, B, C](self: Union[A, B, C]) extends Union[A, B, Option[C]]

  final case class Root[A, +B <: Schema[A, ?, C], C](schema: B) extends Union[A, B, C]

  final case class Transform[A, B, C, D](self: Union[A, B, C], f: C => D, g: D => C) extends Union[A, B, D]
