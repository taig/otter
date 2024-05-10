package io.taig.otter

import cats.data.Chain

sealed trait Tuple[+Of, A] extends Schema[Of, A], Tuple.Reader[Of, A], Tuple.Writer[Of, A]:
  self =>

  final override def reader: Tuple.Reader[Of, A] = this

  final override def writer: Tuple.Writer[Of, A] = this

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
