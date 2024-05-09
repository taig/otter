package io.taig.otter

sealed abstract class Primitive[A] extends Schema[Nothing, A], Primitive.Reader[A], Primitive.Writer[A]:
  override def reader: Primitive.Reader[A]
  override def writer: Primitive.Writer[A]

  override def ivalidate[B, C, D](validation: SchemaValidation[A, B, C, D])(
      f: D => A
  ): Primitive[D] = Primitive.Optional(reader.validate(validation), writer.contramap(f))

  override def optional: Primitive[Option[A]] = Primitive.Optional(reader.optional, writer.optional)

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
