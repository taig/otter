package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Schema.Reader
import cats.data.Chain
import cats.~>
import io.taig.otter.validation.Validation
import cats.Functor

sealed trait Schema[+F[+_], +A, B] extends Schema.Reader[F, A, B], Schema.Writer[F, A, B]:
  def imap[C](f: B => C)(g: C => B): Schema[F, A, C]
  override def optional: Schema[F, A, Option[B]]
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Schema[G, ?, B] = ???

object Schema:
  sealed trait Reader[+F[+_], +A, +B] extends Product, Serializable:
    def map[C](f: B => C): Schema.Reader[F, A, C]
    def optional: Schema.Reader[F, A, Option[B]]
    def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Schema.Reader[G, ?, B] = ???

  sealed trait Writer[+F[+_], +A, -B] extends Product, Serializable:
    def contramap[C](f: C => B): Schema.Writer[F, A, C]
    def optional: Schema.Writer[F, A, Option[B]]
    def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Schema.Writer[G, ?, B] = ???

sealed trait Collection[+F[+_], +A, B] extends Schema[F, A, B], Collection.Reader[F, A, B], Collection.Writer[F, A, B]:
  final override def imap[C](f: B => C)(g: C => B): Collection[F, A, C] = ivalidate(Validation.lift(f))(g)
  final def ivalidate[C, D](validation: SchemaValidation.Collection[B, C, D])(f: D => B): Collection[F, A, D] =
    Collection.Transform(this, validation, f)
  final override def optional: Collection[F, A, Option[B]] = Collection.Optional(this)
  override def schema: F[Schema[F, ?, ?]]
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection[G, ?, B] = ???

object Collection:
  sealed trait Reader[+F[+_], +A, +B] extends Schema.Reader[F, A, B]:
    def constraints: Chain[Constraint.Collection]
    final override def map[C](f: B => C): Collection.Reader[F, A, C] = validate(Validation.lift(f))
    override def optional: Collection.Reader[F, A, Option[B]] = Reader.Optional(this)
    def schema: F[Schema.Reader[F, ?, ?]]
    final def validate[B1 >: B, C, D](
        validation: SchemaValidation.Collection[B1, C, D]
    ): Collection.Reader[F, A, D] = Reader.Transform(this, validation)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection.Reader[G, ?, B] = ???

  object Reader:
    final case class Transform[+F[+_], A, B, C, D](
        self: Collection.Reader[F, A, B],
        validation: SchemaValidation.Collection[B, C, D]
    ) extends Collection.Reader[F, A, D]:
      export self.schema
      override def constraints: Chain[Constraint.Collection] = self.constraints ++ validation.constraints
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection.Reader[G, ?, D] = ???

    final case class Optional[+F[+_], A, B](self: Collection.Reader[F, A, B])
        extends Collection.Reader[F, A, Option[B]]:
      export self.{constraints, schema}
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection.Reader[G, ?, Option[B]] =
        copy(self = self.translate(fK))

    final case class Root[F[+_], +A <: Schema.Reader[F, ?, B], B](schema: F[A])
        extends Collection.Reader[F, F[A], Vector[B]]:
      override def constraints: Chain[Constraint.Collection] = Chain.empty
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection.Reader[G, ?, Vector[B]] =
        copy(schema = fK(schema).map(_.translate(fK)))

  sealed trait Writer[+F[+_], +A, -B] extends Schema.Writer[F, A, B]:
    final def contramap[C](f: C => B): Collection.Writer[F, A, C] = Writer.Transform(this, f)
    def optional: Collection.Writer[F, A, Option[B]] = Writer.Optional(this)
    def schema: F[Schema.Writer[F, ?, ?]]

  object Writer:
    final case class Transform[+F[+_], A, B, C](
        self: Collection.Writer[F, A, B],
        f: C => B
    ) extends Collection.Writer[F, A, C]:
      export self.schema

    final case class Optional[+F[+_], A, B](self: Collection.Writer[F, A, B])
        extends Collection.Writer[F, A, Option[B]]:
      export self.schema

    final case class Root[F[+_], +A <: Schema.Writer[F, ?, B], B](schema: F[A])
        extends Collection.Writer[F, F[A], Vector[B]]

  final case class Optional[+F[+_], A, B](self: Collection[F, A, B]) extends Collection[F, A, Option[B]]:
    export self.{constraints, schema}

  final case class Root[F[+_], +A <: F[Schema[F, ?, B]], B](schema: A) extends Collection[F, F[A], Vector[B]]:
    override def constraints: Chain[Constraint.Collection] = Chain.empty

  final case class Transform[+F[+_], A, B, C, D](
      self: Collection[F, A, B],
      validation: SchemaValidation.Collection[B, C, D],
      f: D => B
  ) extends Collection[F, A, D]:
    export self.schema
    override def constraints: Chain[Constraint.Collection] = self.constraints ++ validation.constraints

sealed trait Primitive[+F[+_], A] extends Schema[F, Nothing, A], Primitive.Reader[F, A], Primitive.Writer[F, A]:
  override def imap[C](f: A => C)(g: C => A): Primitive[F, C] = ivalidate(Validation.lift(f))(g)
  final override def optional: Primitive[F, Option[A]] = Primitive.Optional(this)
  def ivalidate[B, C, D](validation: SchemaValidation.Primitive[A, B, C, D])(
      f: D => A
  ): Primitive[F, D] = Primitive.Transform(this, validation, f)

object Primitive:
  sealed trait Required[+F[+_], A]
      extends Primitive[F, A],
        Primitive.Required.Reader[F, A],
        Primitive.Required.Writer[F, A]:
    final override def imap[C](f: A => C)(g: C => A): Primitive.Required[F, C] = ivalidate(Validation.lift(f))(g)
    override def ivalidate[B, C, D](validation: SchemaValidation.Primitive[A, B, C, D])(
        f: D => A
    ): Primitive.Required[F, D] = Required.Transform(this, validation, f)

  object Required:
    sealed trait Reader[+F[+_], +A] extends Primitive.Reader[F, A]:
      final override def map[C](f: A => C): Primitive.Required.Reader[F, C] = validate(Validation.lift(f))
      final override def validate[A1 >: A, B, C, D](
          transformation: SchemaValidation.Primitive[A1, B, C, D]
      ): Primitive.Required.Reader[F, D] = Reader.Transform(this, transformation)

    object Reader:
      final case class Transform[+F[+_], A, B, C, D](
          self: Primitive.Required.Reader[F, A],
          validation: SchemaValidation.Primitive[A, B, C, D]
      ) extends Primitive.Required.Reader[F, D]:
        export self.tpe
        override def constraints: Chain[Constraint.Primitive[?]] = self.constraints ++ validation.constraints

    sealed trait Writer[+F[+_], -A] extends Primitive.Writer[F, A]:
      final override def contramap[B](f: B => A): Primitive.Required.Writer[F, B] = Writer.Transform(this, f)

    object Writer:
      final case class Transform[+F[+_], A, B](self: Primitive.Required.Writer[F, A], f: B => A)
          extends Primitive.Required.Writer[F, B]:
        export self.tpe

    final case class Root[+F[+_], A](tpe: Type[A]) extends Primitive.Required[F, A]:
      override def constraints: Chain[Constraint.Primitive[?]] = Chain.empty

    final case class Transform[+F[+_], A, B, C, D](
        self: Primitive.Required[F, A],
        validation: SchemaValidation.Primitive[A, B, C, D],
        f: D => A
    ) extends Primitive.Required[F, D]:
      export self.tpe
      override def constraints: Chain[Constraint.Primitive[?]] = self.constraints ++ validation.constraints

  sealed trait Reader[+F[+_], +A] extends Schema.Reader[F, Nothing, A]:
    def constraints: Chain[Constraint.Primitive[?]]
    override def map[C](f: A => C): Primitive.Reader[F, C] = validate(Validation.lift(f))
    override def optional: Primitive.Reader[F, Option[A]] = Reader.Optional(this)
    def tpe: Type[?]
    def validate[A1 >: A, B, C, D](
        validation: SchemaValidation.Primitive[A1, B, C, D]
    ): Primitive.Reader[F, D] = Reader.Transform(this, validation)

  object Reader:
    final case class Transform[+F[+_], A, B, C, D](
        self: Primitive.Reader[F, A],
        validation: SchemaValidation.Primitive[A, B, C, D]
    ) extends Primitive.Reader[F, D]:
      export self.tpe
      override def constraints: Chain[Constraint.Primitive[?]] = self.constraints ++ validation.constraints

    final case class Optional[+F[+_], A](self: Primitive.Reader[F, A]) extends Primitive.Reader[F, Option[A]]:
      export self.{constraints, tpe}

  sealed trait Writer[+F[+_], -A] extends Schema.Writer[F, Nothing, A]:
    override def contramap[B](f: B => A): Primitive.Writer[F, B] = Writer.Transform(this, f)
    override def optional: Primitive.Writer[F, Option[A]] = Writer.Optional(this)
    def tpe: Type[?]

  object Writer:
    final case class Transform[+F[+_], A, B](self: Primitive.Writer[F, A], f: B => A) extends Primitive.Writer[F, B]:
      export self.tpe

    final case class Optional[+F[+_], A](self: Primitive.Writer[F, A]) extends Primitive.Writer[F, Option[A]]:
      export self.tpe

  final case class Optional[+F[+_], A](self: Primitive[F, A]) extends Primitive[F, Option[A]]:
    export self.{constraints, tpe}

  final case class Transform[+F[+_], A, B, C, D](
      self: Primitive[F, A],
      validation: SchemaValidation.Primitive[A, B, C, D],
      f: D => A
  ) extends Primitive[F, D]:
    export self.tpe
    override def constraints: Chain[Constraint.Primitive[?]] = self.constraints ++ validation.constraints

sealed trait Union[+F[+_], +A, B] extends Schema[F, A, B], Union.Reader[F, A, B], Union.Writer[F, A, B]:
  override def imap[C](f: B => C)(g: C => B): Union[F, A, C] = ???
  override def optional: Union[F, A, Option[B]] = ???
  // def orElse[C, D](union: Union[F, C, D]): Union[F, A | C, Either[B, D]] = ???

object Union:
  sealed trait Reader[+F[+_], +A, +B] extends Schema.Reader[F, A, B]:
    final override def map[C](f: B => C): Union.Reader[F, A, C] = ???
    override def optional: Union.Reader[F, A, Option[B]] = ???

  object Reader:
    final case class OrElse[+F[+_], A, B, C, D](left: Union.Reader[F, A, B], right: Union.Reader[F, A, B])
        extends Union.Reader[F, A | C, Either[B, D]]

    final case class Root[+F[+_], A <: F[Schema.Reader[F, ?, B]], B](schema: A) extends Union.Reader[F, A, B]

  sealed trait Writer[+F[+_], +A, -B] extends Schema.Writer[F, A, B]:
    final override def contramap[C](f: C => B): Union.Writer[F, A, C] = ???
    override def optional: Union.Writer[F, A, Option[B]] = ???

  object Writer:
    final case class OrElse[+F[+_], A, B, C, D](left: Union.Writer[F, A, B], right: Union.Writer[F, A, B])
        extends Union.Writer[F, A | C, Either[B, D]]

    final case class Root[+F[+_], A <: F[Schema.Writer[F, ?, B]], B](schema: A) extends Union.Writer[F, A, B]

  final case class OrElse[+F[+_], A, B, C, D](left: Union[F, A, B], right: Union[F, C, D])
      extends Union[F, A | C, Either[B, D]]

  final case class Root[+F[+_], A <: F[Schema[F, ?, B]], B](schema: A) extends Union[F, A, B]
