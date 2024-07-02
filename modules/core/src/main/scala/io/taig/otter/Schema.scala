package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Schema.Reader
import cats.data.Chain
import io.taig.otter.validation.Validation
import cats.Functor
import io.taig.otter.Union.Writer

sealed trait Schema[+F[+_], +A, B] extends Schema.Reader[F, A, B], Schema.Writer[F, A, B]:
  def imap[C](f: B => C)(g: C => B): Schema[F, A, C]
  override def optional: Schema[F, A, Option[B]]
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Schema[G, ?, B]

object Schema:
  sealed trait Reader[+F[+_], +A, +B] extends Product, Serializable:
    def map[C](f: B => C): Schema.Reader[F, A, C]
    def optional: Schema.Reader[F, A, Option[B]]
    def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Schema.Reader[G, ?, B]

  sealed trait Writer[+F[+_], +A, -B] extends Product, Serializable:
    def contramap[C](f: C => B): Schema.Writer[F, A, C]
    def optional: Schema.Writer[F, A, Option[B]]
    def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Schema.Writer[G, ?, B]

sealed trait Collection[+F[+_], +A, B] extends Schema[F, A, B], Collection.Reader[F, A, B], Collection.Writer[F, A, B]:
  final override def imap[C](f: B => C)(g: C => B): Collection[F, A, C] = ivalidate(Validation.lift(f))(g)
  final def ivalidate[C, D](validation: SchemaValidation.Collection[B, C, D])(f: D => B): Collection[F, A, D] =
    Collection.Transform(this, validation, f)
  final override def optional: Collection[F, A, Option[B]] = Collection.Optional(this)
  override def schema: F[Schema[F, ?, ?]]
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection[G, ?, B]

object Collection:
  sealed trait Reader[+F[+_], +A, +B] extends Schema.Reader[F, A, B]:
    def constraints: Chain[Constraint.Collection]
    final override def map[C](f: B => C): Collection.Reader[F, A, C] = validate(Validation.lift(f))
    override def optional: Collection.Reader[F, A, Option[B]] = Reader.Optional(this)
    def schema: F[Schema.Reader[F, ?, ?]]
    final def validate[B1 >: B, C, D](
        validation: SchemaValidation.Collection[B1, C, D]
    ): Collection.Reader[F, A, D] = Reader.Transform(this, validation)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection.Reader[G, ?, B]

  object Reader:
    final case class Transform[+F[+_], A, B, C, D](
        self: Collection.Reader[F, A, B],
        validation: SchemaValidation.Collection[B, C, D]
    ) extends Collection.Reader[F, A, D]:
      export self.schema
      override def constraints: Chain[Constraint.Collection] = self.constraints ++ validation.constraints
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection.Reader[G, ?, D] =
        copy(self = self.translate(fK))

    final case class Optional[+F[+_], A, B](self: Collection.Reader[F, A, B])
        extends Collection.Reader[F, A, Option[B]]:
      export self.{constraints, schema}
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection.Reader[G, ?, Option[B]] =
        copy(self = self.translate(fK))

    final case class Root[F[+_], +A <: F[Schema.Reader[F, ?, B]], B](schema: A)
        extends Collection.Reader[F, A, Vector[B]]:
      override def constraints: Chain[Constraint.Collection] = Chain.empty
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection.Reader[G, ?, Vector[B]] =
        copy(schema = fK(schema).map(_.translate(fK)))

  sealed trait Writer[+F[+_], +A, -B] extends Schema.Writer[F, A, B]:
    final def contramap[C](f: C => B): Collection.Writer[F, A, C] = Writer.Transform(this, f)
    def optional: Collection.Writer[F, A, Option[B]] = Writer.Optional(this)
    def schema: F[Schema.Writer[F, ?, ?]]
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection.Writer[G, ?, B]

  object Writer:
    final case class Transform[+F[+_], A, B, C](
        self: Collection.Writer[F, A, B],
        f: C => B
    ) extends Collection.Writer[F, A, C]:
      export self.schema
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection.Writer[G, ?, C] =
        copy(self = self.translate(fK))

    final case class Optional[+F[+_], A, B](self: Collection.Writer[F, A, B])
        extends Collection.Writer[F, A, Option[B]]:
      export self.schema
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection.Writer[G, ?, Option[B]] =
        copy(self = self.translate(fK))

    final case class Root[F[+_], +A <: F[Schema.Writer[F, ?, B]], B](schema: A)
        extends Collection.Writer[F, A, Vector[B]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection.Writer[G, ?, Vector[B]] =
        copy(schema = fK(schema).map(_.translate(fK)))

  final case class Optional[+F[+_], A, B](self: Collection[F, A, B]) extends Collection[F, A, Option[B]]:
    export self.{constraints, schema}
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection[G, ?, Option[B]] =
      copy(self = self.translate(fK))

  final case class Root[F[+_], +A <: F[Schema[F, ?, B]], B](schema: A) extends Collection[F, A, Vector[B]]:
    override def constraints: Chain[Constraint.Collection] = Chain.empty
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection[G, ?, Vector[B]] =
      copy(schema = fK(schema).map(_.translate(fK)))

  final case class Transform[+F[+_], A, B, C, D](
      self: Collection[F, A, B],
      validation: SchemaValidation.Collection[B, C, D],
      f: D => B
  ) extends Collection[F, A, D]:
    export self.schema
    override def constraints: Chain[Constraint.Collection] = self.constraints ++ validation.constraints
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection[G, ?, D] =
      copy(self = self.translate(fK))

sealed trait Primitive[A] extends Schema[Nothing, Nothing, A], Primitive.Reader[A], Primitive.Writer[A]:
  override def imap[C](f: A => C)(g: C => A): Primitive[C] = ivalidate(Validation.lift(f))(g)
  final override def optional: Primitive[Option[A]] = Primitive.Optional(this)
  def ivalidate[B, C, D](validation: SchemaValidation.Primitive[A, B, C, D])(
      f: D => A
  ): Primitive[D] = Primitive.Transform(this, validation, f)
  override def translate[G[+_]: Functor](fK: [A] => Nothing => G[A]): Primitive[A] = this

object Primitive:
  trait Ops:
    def tpe: Type[?]

  sealed trait Required[A] extends Primitive[A], Primitive.Required.Reader[A], Primitive.Required.Writer[A]:
    final override def imap[C](f: A => C)(g: C => A): Primitive.Required[C] = ivalidate(Validation.lift(f))(g)
    override def ivalidate[B, C, D](validation: SchemaValidation.Primitive[A, B, C, D])(
        f: D => A
    ): Primitive.Required[D] = Required.Transform(this, validation, f)
    final override def translate[G[+_]: Functor](fK: [A] => Nothing => G[A]): Primitive.Required[A] = this

  object Required:
    sealed trait Reader[+A] extends Primitive.Reader[A]:
      final override def map[C](f: A => C): Primitive.Required.Reader[C] = validate(Validation.lift(f))
      final override def validate[A1 >: A, B, C, D](
          transformation: SchemaValidation.Primitive[A1, B, C, D]
      ): Primitive.Required.Reader[D] = Reader.Transform(this, transformation)
      override def translate[G[+_]: Functor](fK: [A] => Nothing => G[A]): Primitive.Required.Reader[A] = this

    object Reader:
      final case class Transform[A, B, C, D](
          self: Primitive.Required.Reader[A],
          validation: SchemaValidation.Primitive[A, B, C, D]
      ) extends Primitive.Required.Reader[D]:
        export self.tpe
        override def constraints: Chain[Constraint.Primitive[?]] = self.constraints ++ validation.constraints

    sealed trait Writer[-A] extends Primitive.Writer[A]:
      final override def contramap[B](f: B => A): Primitive.Required.Writer[B] = Writer.Transform(this, f)
      override def translate[G[+_]: Functor](fK: [A] => Nothing => G[A]): Primitive.Required.Writer[A] = this

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

  sealed trait Reader[+A] extends Schema.Reader[Nothing, Nothing, A], Primitive.Ops:
    def constraints: Chain[Constraint.Primitive[?]]
    override def map[C](f: A => C): Primitive.Reader[C] = validate(Validation.lift(f))
    override def optional: Primitive.Reader[Option[A]] = Reader.Optional(this)
    def validate[A1 >: A, B, C, D](
        validation: SchemaValidation.Primitive[A1, B, C, D]
    ): Primitive.Reader[D] = Reader.Transform(this, validation)
    override def translate[G[+_]: Functor](fK: [A] => Nothing => G[A]): Primitive.Reader[A] = this

  object Reader:
    final case class Transform[A, B, C, D](
        self: Primitive.Reader[A],
        validation: SchemaValidation.Primitive[A, B, C, D]
    ) extends Primitive.Reader[D]:
      export self.tpe
      override def constraints: Chain[Constraint.Primitive[?]] = self.constraints ++ validation.constraints

    final case class Optional[+F[+_], A](self: Primitive.Reader[A]) extends Primitive.Reader[Option[A]]:
      export self.{constraints, tpe}

  sealed trait Writer[-A] extends Schema.Writer[Nothing, Nothing, A], Primitive.Ops:
    override def contramap[B](f: B => A): Primitive.Writer[B] = Writer.Transform(this, f)
    override def optional: Primitive.Writer[Option[A]] = Writer.Optional(this)
    override def translate[G[+_]: Functor](fK: [A] => Nothing => G[A]): Primitive.Writer[A] = this

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

sealed trait Union[+F[+_], +A, B] extends Schema[F, A, B], Union.Reader[F, A, B], Union.Writer[F, A, B]:
  override def imap[C](f: B => C)(g: C => B): Union[F, A, C] = Union.Transform(this, f, g)
  override def optional: Union[F, A, Option[B]] = ???
  def orElse[G[+a] >: F[a], C, D](union: Union[G, C, D]): Union[G, A | C, Either[B, D]] = Union.OrElse(this, union)
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union[G, ?, B]

object Union:
  sealed trait Reader[+F[+_], +A, +B] extends Schema.Reader[F, A, B]:
    final override def map[C](f: B => C): Union.Reader[F, A, C] = ???
    override def optional: Union.Reader[F, A, Option[B]] = Reader.Optional(this)
    def orElse[G[+a] >: F[a], C, D](union: Union.Reader[G, C, D]): Union.Reader[G, A | C, Either[B, D]] =
      Reader.OrElse(this, union)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Reader[G, ?, B]

  object Reader:
    final case class Optional[F[+_], A, B](self: Union.Reader[F, A, B]) extends Union.Reader[F, A, Option[B]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Reader[G, ?, Option[B]] =
        copy(self = self.translate(fK))

    final case class OrElse[F[+_], A, B, C, D](left: Union.Reader[F, A, B], right: Union.Reader[F, C, D])
        extends Union.Reader[F, A | C, Either[B, D]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Reader[G, ?, Either[B, D]] =
        copy(left = left.translate(fK), right = right.translate(fK))

    final case class Root[+F[+_], A <: F[Schema.Reader[F, ?, B]], B](schema: A) extends Union.Reader[F, A, B]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Reader[G, ?, B] =
        copy(schema = fK(schema).map(_.translate(fK)))

  sealed trait Writer[+F[+_], +A, -B] extends Schema.Writer[F, A, B]:
    final override def contramap[C](f: C => B): Union.Writer[F, A, C] = ???
    override def optional: Union.Writer[F, A, Option[B]] = Writer.Optional(this)
    def orElse[G[+a] >: F[a], C, D](union: Union.Writer[G, C, D]): Union.Writer[G, A | C, Either[B, D]] =
      Writer.OrElse(this, union)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Writer[G, ?, B]

  object Writer:
    final case class Optional[F[+_], A, B](self: Union.Writer[F, A, B]) extends Union.Writer[F, A, Option[B]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Writer[G, ?, Option[B]] =
        copy(self = self.translate(fK))

    final case class OrElse[F[+_], A, B, C, D](left: Union.Writer[F, A, B], right: Union.Writer[F, C, D])
        extends Union.Writer[F, A | C, Either[B, D]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Writer[G, ?, Either[B, D]] =
        copy(left = left.translate(fK), right = right.translate(fK))

    final case class Root[F[+_], +A <: F[Schema.Writer[F, ?, B]], B](schema: A) extends Union.Writer[F, A, B]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Writer[G, ?, B] =
        copy(schema = fK(schema).map(_.translate(fK)))

  final case class OrElse[F[+_], A, B, C, D](left: Union[F, A, B], right: Union[F, C, D])
      extends Union[F, A | C, Either[B, D]]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union[G, ?, Either[B, D]] =
      copy(left = left.translate(fK), right = right.translate(fK))

  final case class Root[+F[+_], +A <: F[Schema[F, ?, B]], B](schema: A) extends Union[F, A, B]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union[G, ?, B] =
      copy(schema = fK(schema).map(_.translate(fK)))

  final case class Transform[+F[+_], A, B, C](self: Union[F, A, B], f: B => C, g: C => B) extends Union[F, A, C]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union[G, ?, C] =
      copy(self = self.translate(fK))
