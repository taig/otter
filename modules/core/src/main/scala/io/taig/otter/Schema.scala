package io.taig.otter

import cats.data.Chain
import io.taig.otter.Schema.Reader

sealed trait Schema[+Of, A] extends Schema.Reader[Of, A], Schema.Writer[Of, A]:
  def ivalidate[B, C, D](validation: SchemaValidation[A, B, C, D])(f: D => A): Schema[Of, D]
  def optional: Schema[Of, Option[A]]

object Schema:
  sealed trait Reader[+Of, +A]:
    def validate[B, C, D](validation: SchemaValidation[A, B, C, D]): Schema.Reader[Of, D]
    def optional: Schema.Reader[Of, Option[A]]

  object Reader:
    given [Of]: SchemaFunctor[Schema.Reader[Of, *], Schema.Reader[Of, *]] with
      override def validate[A, B, C, D](fa: Schema.Reader[Of, A])(
          validation: SchemaValidation[A, B, C, D]
      ): Schema.Reader[Of, D] = fa.validate(validation)
      override def optional[A](fa: Schema.Reader[Of, A]): Schema.Reader[Of, Option[A]] = fa.optional

  sealed trait Writer[+Of, -A]:
    def contramap[B](f: B => A): Schema.Writer[Of, B]
    def optional: Schema.Writer[Of, Option[A]]

  object Writer:
    given [Of]: SchemaContravariant[Schema.Writer[Of, *], Schema.Writer[Of, *]] with
      override def contramap[A, B](fa: Schema.Writer[Of, A])(f: B => A): Schema.Writer[Of, B] = fa.contramap(f)
      override def optional[A](fa: Schema.Writer[Of, A]): Schema.Writer[Of, Option[A]] = fa.optional

  given [Of]: SchemaInvariant[Schema[Of, *], Schema[Of, *]] with
    override def ivalidate[A, B, C, D](fa: Schema[Of, A])(validation: SchemaValidation[A, B, C, D])(
        f: D => A
    ): Schema[Of, D] = fa.ivalidate(validation)(f)
    override def optional[A](fa: Schema[Of, A]): Schema[Of, Option[A]] = fa.optional

sealed trait Collection[+Of, A] extends Schema[Of, A], Collection.Reader[Of, A], Collection.Writer[Of, A]:
  final override def ivalidate[B, C, D](validation: SchemaValidation[A, B, C, D])(f: D => A): Schema[Of, D] =
    Collection.Validate(this, validation, f)
  final override def optional: Collection[Of, Option[A]] = Collection.Optional(this)

object Collection:
  trait Operation[+Of]:
    def schema: Of

  sealed trait Reader[+Of, +A] extends Schema.Reader[Of, A], Collection.Operation[Of]:
    override def optional: Collection.Reader[Of, Option[A]] = Reader.Optional(this)
    final override def validate[B, C, D](validation: SchemaValidation[A, B, C, D]): Collection.Reader[Of, D] =
      Reader.Validate(this, validation)

  object Reader:
    final case class Optional[Of, A](self: Collection.Reader[Of, A]) extends Collection.Reader[Of, Option[A]]:
      export self.schema
    final case class Root[Of, A](schema: Of, reader: Of => Schema.Reader[Of, A]) extends Collection.Reader[Of, Chain[A]]
    final case class Validate[Of, A, B, C, D](self: Collection.Reader[Of, A], validation: SchemaValidation[A, B, C, D])
        extends Collection.Reader[Of, D]:
      export self.schema

  sealed trait Writer[+Of, -A] extends Schema.Writer[Of, A], Collection.Operation[Of]:
    final override def contramap[B](f: B => A): Collection.Writer[Of, B] = Writer.Modify(this, f)
    override def optional: Collection.Writer[Of, Option[A]] = Writer.Optional(this)

  object Writer:
    final case class Modify[Of, A, B](self: Collection.Writer[Of, A], f: B => A) extends Collection.Writer[Of, B]:
      export self.schema
    final case class Optional[Of, A](self: Collection.Writer[Of, A]) extends Collection.Writer[Of, Option[A]]:
      export self.schema
    final case class Root[Of, A](schema: Of, writer: Of => Schema.Writer[Of, A]) extends Collection.Writer[Of, Chain[A]]

  final case class Optional[Of, A](self: Collection[Of, A]) extends Collection[Of, Option[A]]:
    export self.schema

  final case class Validate[Of, A, B, C, D](
      self: Collection[Of, A],
      validation: SchemaValidation[A, B, C, D],
      f: D => A
  ) extends Collection[Of, D]:
    export self.schema

  final case class Root[Of, A](schema: Of, base: Of => Schema[Of, A]) extends Collection[Of, Chain[A]]

sealed abstract class Primitive[A] extends Schema[Nothing, A], Primitive.Reader[A], Primitive.Writer[A]:
  override def ivalidate[B, C, D](validation: SchemaValidation[A, B, C, D])(
      f: D => A
  ): Primitive[D] = ???

  override def optional: Primitive[Option[A]] = ???

object Primitive:
  trait Operation:
    def tpe: Type[?]

  final case class Required[A] private (reader: Primitive.Required.Reader[A], writer: Primitive.Required.Writer[A])
      extends Primitive[A],
        Primitive.Required.Reader[A],
        Primitive.Required.Writer[A]:
    export reader.tpe
    override def ivalidate[B, C, D](validation: SchemaValidation[A, B, C, D])(
        f: D => A
    ): Primitive.Required[D] = Required(reader.validate(validation), writer.contramap(f))
    override def optional: Primitive[Option[A]] = Primitive.Optional(reader.optional, writer.optional)

  object Required:
    sealed trait Reader[+A] extends Primitive.Reader[A]:
      final override def validate[B, C, D](
          validation: SchemaValidation[A, B, C, D]
      ): Primitive.Required.Reader[D] = Reader.Validate(this, validation)
      override def optional: Primitive.Reader[Option[A]] = Primitive.Reader.Optional(this)

    object Reader:
      final case class Validate[A, B, C, D](
          self: Primitive.Required.Reader[A],
          validation: SchemaValidation[A, B, C, D]
      ) extends Primitive.Required.Reader[D]:
        export self.tpe

      final case class Root[A](tpe: Type[A]) extends Primitive.Required.Reader[A]

      given PrimitiveFunctor[Primitive.Required.Reader, Primitive.Reader] with
        override def optional[A](fa: Primitive.Required.Reader[A]): Primitive.Reader[Option[A]] =
          fa.optional
        override def validate[A, B, C, D](fa: Primitive.Required.Reader[A])(
            validation: SchemaValidation[A, B, C, D]
        ): Reader[D] = fa.validate(validation)
        override def tpe[A](fa: Primitive.Required.Reader[A]): Type[?] = fa.tpe

    sealed trait Writer[-A] extends Primitive.Writer[A]:
      final override def contramap[B](f: B => A): Primitive.Required.Writer[B] = Writer.Modify(this, f)
      override def optional: Primitive.Writer[Option[A]] = Primitive.Writer.Optional(this)

    object Writer:
      final case class Modify[A, B](self: Primitive.Required.Writer[A], f: B => A) extends Primitive.Required.Writer[B]:
        export self.tpe

      final case class Root[A](tpe: Type[A]) extends Primitive.Required.Writer[A]

      given PrimitiveContravariant[Primitive.Required.Writer, Primitive.Writer] with
        override def optional[A](fa: Primitive.Required.Writer[A]): Primitive.Writer[Option[A]] = fa.optional
        override def tpe[A](fa: Primitive.Required.Writer[A]): Type[?] = fa.tpe
        override def contramap[A, B](fa: Primitive.Required.Writer[A])(f: B => A): Primitive.Required.Writer[B] =
          fa.contramap(f)

    def apply[A](tpe: Type[A]): Primitive.Required[A] = Required(Reader.Root(tpe), Writer.Root(tpe))

    given PrimitiveInvariant[Primitive.Required, Primitive] with
      override def optional[A](fa: Primitive.Required[A]): Primitive[Option[A]] = fa.optional
      override def ivalidate[A, B, C, D](fa: Primitive.Required[A])(validation: SchemaValidation[A, B, C, D])(
          f: D => A
      ): Primitive.Required[D] = fa.ivalidate(validation)(f)
      override def tpe[A](fa: Primitive.Required[A]): Type[?] = fa.tpe

  final case class Optional[A] private[otter] (reader: Primitive.Reader[A], writer: Primitive.Writer[A])
      extends Primitive[A]:
    export reader.tpe

  sealed trait Reader[+A] extends Schema.Reader[Nothing, A], Primitive.Operation:
    override def validate[B, C, D](validation: SchemaValidation[A, B, C, D]): Primitive.Reader[D] =
      Reader.Validate(this, validation)
    override def optional: Primitive.Reader[Option[A]] = Reader.Optional(this)

  object Reader:
    final case class Validate[A, B, C, D](self: Primitive.Reader[A], validation: SchemaValidation[A, B, C, D])
        extends Primitive.Reader[D]:
      export self.tpe

    final case class Optional[A](self: Primitive.Reader[A]) extends Primitive.Reader[Option[A]]:
      export self.tpe

    given PrimitiveFunctor[Primitive.Reader, Primitive.Reader] with
      override def optional[A](fa: Primitive.Reader[A]): Primitive.Reader[Option[A]] = fa.optional
      override def tpe[A](fa: Primitive.Reader[A]): Type[?] = fa.tpe
      override def validate[A, B, C, D](fa: Primitive.Reader[A])(
          validation: SchemaValidation[A, B, C, D]
      ): Primitive.Reader[D] = fa.validate(validation)

  sealed trait Writer[-A] extends Schema.Writer[Nothing, A], Primitive.Operation:
    override def contramap[B](f: B => A): Primitive.Writer[B] = Writer.Modify(this, f)
    override def optional: Primitive.Writer[Option[A]] = Writer.Optional(this)

  object Writer:
    final case class Modify[A, B](self: Primitive.Writer[A], f: B => A) extends Primitive.Writer[B]:
      export self.tpe

    final case class Optional[A](self: Primitive.Writer[A]) extends Primitive.Writer[Option[A]]:
      export self.tpe

    given PrimitiveContravariant[Primitive.Writer, Primitive.Writer] with
      override def optional[A](fa: Writer[A]): Writer[Option[A]] = fa.optional
      override def tpe[A](fa: Writer[A]): Type[?] = fa.tpe
      override def contramap[A, B](fa: Writer[A])(f: B => A): Writer[B] = fa.contramap(f)

  given PrimitiveInvariant[Primitive, Primitive] with

    override def optional[A](fa: Primitive[A]): Primitive[Option[A]] = fa.optional

    override def ivalidate[A, B, C, D](fa: Primitive[A])(validation: SchemaValidation[A, B, C, D])(
        f: D => A
    ): Primitive[D] = fa.ivalidate(validation)(f)

    override def tpe[A](fa: Primitive[A]): Type[?] = fa.tpe

sealed trait Tuple[+Of, A] extends Schema[Of, A], Tuple.Reader[Of, A], Tuple.Writer[Of, A]:
  self =>

  final override def ivalidate[B, C, D](validation: SchemaValidation[A, B, C, D])(f: D => A): Tuple[Of, D] =
    Tuple.Validate(this, validation, f)

  final override def optional: Tuple[Of, Option[A]] = Tuple.Optional(this)

  final def product[Of, B](tuple: Tuple[Of, B]): Tuple[self.Of | Of, (A, B)] = Tuple.Product(this, tuple)

object Tuple:
  trait Operation[+Of]:
    def schemas: Chain[Of]

  sealed trait Reader[+Of, +A] extends Schema.Reader[Of, A], Tuple.Operation[Of]:
    self =>

    override def optional: Tuple.Reader[Of, Option[A]] = Reader.Optional(this)

    final def product[Of, B](tuple: Tuple.Reader[Of, B]): Tuple.Reader[self.Of | Of, (A, B)] =
      Reader.Product(this, tuple)

    final override def validate[B, C, D](validation: SchemaValidation[A, B, C, D]): Tuple.Reader[Of, D] =
      Reader.Validate(this, validation)

  object Reader:
    case object Empty extends Tuple.Reader[Nothing, Unit]:
      override def schemas: Chain[Nothing] = Chain.empty

    final case class Optional[Of, A](self: Tuple.Reader[Of, A]) extends Tuple.Reader[Of, Option[A]]:
      export self.schemas

    final case class Product[OfA, A, OfB, B](left: Tuple.Reader[OfA, A], right: Tuple.Reader[OfB, B])
        extends Tuple.Reader[OfA | OfB, (A, B)]:
      override def schemas: Chain[OfA | OfB] = left.schemas ++ right.schemas

    final case class One[S[_], A](schema: S[A]) extends Tuple.Reader[S[A], A]:
      override def schemas: Chain[S[A]] = Chain.one(schema)

    final case class Validate[Of, A, B, C, D](self: Tuple.Reader[Of, A], validation: SchemaValidation[A, B, C, D])
        extends Tuple.Reader[Of, D]:
      export self.schemas

    given [Of]: TupleFunctor[Tuple.Reader, Of] with

      override def optional[A](fa: Tuple.Reader[Of, A]): Tuple.Reader[Of, Option[A]] = fa.optional

      override def schemas[A](fa: Tuple.Reader[Of, A]): Chain[Of] = fa.schemas

      override def validate[A, B, C, D](fa: Tuple.Reader[Of, A])(
          validation: SchemaValidation[A, B, C, D]
      ): Tuple.Reader[Of, D] = fa.validate(validation)

  sealed trait Writer[+Of, -A] extends Schema.Writer[Of, A], Tuple.Operation[Of]:
    self =>

    final override def contramap[B](f: B => A): Tuple.Writer[Of, B] = Writer.Modify(this, f)

    override def optional: Tuple.Writer[Of, Option[A]] = Writer.Optional(this)

    final def product[Of, B](tuple: Tuple.Writer[Of, B]): Tuple.Writer[self.Of | Of, (A, B)] =
      Writer.Product(this, tuple)

  object Writer:
    case object Empty extends Tuple.Writer[Nothing, Unit]:
      override def schemas: Chain[Nothing] = Chain.empty

    final case class Optional[Of, A](self: Tuple.Writer[Of, A]) extends Tuple.Writer[Of, Option[A]]:
      export self.schemas

    final case class Modify[Of, A, B](self: Tuple.Writer[Of, A], f: B => A) extends Tuple.Writer[Of, B]:
      export self.schemas

    final case class Product[OfA, A, OfB, B](left: Tuple.Writer[OfA, A], right: Tuple.Writer[OfB, B])
        extends Tuple.Writer[OfA | OfB, (A, B)]:
      override def schemas: Chain[OfA | OfB] = left.schemas ++ right.schemas

    final case class One[S[_], A](schema: S[A]) extends Tuple.Writer[S[A], A]:
      override def schemas: Chain[S[A]] = Chain.one(schema)

    given [Of]: TupleContravariant[Tuple.Writer, Of] with
      override def optional[A](fa: Tuple.Writer[Of, A]): Tuple.Writer[Of, Option[A]] = fa.optional
      override def schemas[A](fa: Tuple.Writer[Of, A]): Chain[Of] = fa.schemas
      override def contramap[A, B](fa: Tuple.Writer[Of, A])(f: B => A): Tuple.Writer[Of, B] = fa.contramap(f)

  case object Empty extends Tuple[Nothing, Unit]:
    override def schemas: Chain[Nothing] = Chain.empty

  final case class Optional[Of, A](self: Tuple[Of, A]) extends Tuple[Of, Option[A]]:
    export self.schemas

  final case class Validate[Of, A, B, C, D](self: Tuple[Of, A], validation: SchemaValidation[A, B, C, D], f: D => A)
      extends Tuple[Of, D]:
    export self.schemas

  final case class Product[OfA, A, OfB, B](left: Tuple[OfA, A], right: Tuple[OfB, B]) extends Tuple[OfA | OfB, (A, B)]:
    override def schemas: Chain[OfA | OfB] = left.schemas ++ right.schemas

  final case class One[S[_], A](schema: S[A]) extends Tuple[S[A], A]:
    override def schemas: Chain[S[A]] = Chain.one(schema)

  given [Of]: TupleInvariant[Tuple, Of] with
    override def optional[A](fa: Tuple[Of, A]): Tuple[Of, Option[A]] = fa.optional

    override def ivalidate[A, B, C, D](fa: Tuple[Of, A])(validation: SchemaValidation[A, B, C, D])(
        f: D => A
    ): Tuple[Of, D] = fa.ivalidate(validation)(f)
    override def schemas[A](fa: Tuple[Of, A]): Chain[Of] = fa.schemas
