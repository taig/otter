package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Schema.Reader
import cats.data.Chain

sealed trait Schema[F[+_], +A, B] extends Schema.Reader[F, A, B], Schema.Writer[F, A, B]:
  def imap[C](f: B => C)(g: C => B): Schema[F, A, C]
  override def optional: Schema[F, A, Option[B]]

object Schema:
  sealed trait Reader[F[+_], +A, +B] extends Product, Serializable:
    def map[C](f: B => C): Schema.Reader[F, A, C]
    def optional: Schema.Reader[F, A, Option[B]]

  sealed trait Writer[F[+_], +A, -B] extends Product, Serializable:
    def contramap[C](f: C => B): Schema.Writer[F, A, C]
    def optional: Schema.Writer[F, A, Option[B]]

sealed trait Collection[F[+_], +A, B] extends Schema[F, A, B], Collection.Reader[F, A, B], Collection.Writer[F, A, B]:
  final override def imap[C](f: B => C)(g: C => B): Collection[F, A, C] = transform(Transformation.lift(f)(g))
  final def transform[C, D](transformation: CollectionTransformation[F, B, C, D]): Collection[F, A, D] =
    Collection.Transform(this, transformation)
  final override def optional: Collection[F, A, Option[B]] = Collection.Optional(this)
  override def schema: F[Schema[F, ?, ?]]

object Collection:
  sealed trait Reader[F[+_], +A, +B] extends Schema.Reader[F, A, B]:
    def constraints: Chain[Constraint.Collection]
    final override def map[C](f: B => C): Collection.Reader[F, A, C] = transform(Transformation.Reader.lift(f))
    override def optional: Collection.Reader[F, A, Option[B]] = Reader.Optional(this)
    def schema: F[Schema.Reader[F, ?, ?]]
    final def transform[B1 >: B, C, D](
        transformation: CollectionTransformation.Reader[F, B1, C, D]
    ): Collection.Reader[F, A, D] = Reader.Transform(this, transformation)

  object Reader:
    final case class Transform[F[+_], A, B, C, D](
        self: Collection.Reader[F, A, B],
        transformation: CollectionTransformation.Reader[F, B, C, D]
    ) extends Collection.Reader[F, A, D]:
      export self.schema
      override def constraints: Chain[Constraint.Collection] = self.constraints ++ transformation.validation.constraints

    final case class Optional[F[+_], A, B](self: Collection.Reader[F, A, B]) extends Collection.Reader[F, A, Option[B]]:
      export self.{constraints, schema}

    final case class Root[F[+_], A <: F[Schema.Reader[F, ?, B]], B](schema: A)
        extends Collection.Reader[F, A, Vector[B]]:
      override def constraints: Chain[Constraint.Collection] = Chain.empty

  sealed trait Writer[F[+_], +A, -B] extends Schema.Writer[F, A, B]:
    final def contramap[C](f: C => B): Collection.Writer[F, A, C] = transform(Transformation.Writer(f))
    def optional: Collection.Writer[F, A, Option[B]] = Writer.Optional(this)
    def schema: F[Schema.Writer[F, ?, ?]]
    def transform[B1 <: B, C](transformation: Transformation.Writer[B1, C]): Collection.Writer[F, A, C] =
      Writer.Transform(this, transformation)

  object Writer:
    final case class Transform[F[+_], A, B, C](
        self: Collection.Writer[F, A, B],
        transformation: Transformation.Writer[B, C]
    ) extends Collection.Writer[F, A, C]:
      export self.schema

    final case class Optional[F[+_], A, B](self: Collection.Writer[F, A, B]) extends Collection.Writer[F, A, Option[B]]:
      export self.schema

    final case class Root[F[+_], A <: F[Schema.Writer[F, ?, B]], B](schema: A)
        extends Collection.Writer[F, A, Vector[B]]

  final case class Optional[F[+_], A, B](self: Collection[F, A, B]) extends Collection[F, A, Option[B]]:
    export self.{constraints, schema}

  final case class Root[F[+_], A <: F[Schema[F, ?, B]], B](schema: A) extends Collection[F, A, Vector[B]]:
    override def constraints: Chain[Constraint.Collection] = Chain.empty

  final case class Transform[F[+_], A, B, C, D](
      self: Collection[F, A, B],
      transformation: CollectionTransformation[F, B, C, D]
  ) extends Collection[F, A, D]:
    export self.schema
    override def constraints: Chain[Constraint.Collection] = self.constraints ++ transformation.validation.constraints

sealed trait Primitive[F[+_], A] extends Schema[F, Nothing, A], Primitive.Reader[F, A], Primitive.Writer[F, A]:
  override def imap[C](f: A => C)(g: C => A): Primitive[F, C] = transform(Transformation.lift(f)(g))
  final override def optional: Primitive[F, Option[A]] = Primitive.Optional(this)
  def transform[B, C, D](transformation: PrimitiveTransformation[F, A, B, C, D]): Primitive[F, D] =
    Primitive.Transform(this, transformation)

object Primitive:
  sealed trait Required[F[+_], A]
      extends Primitive[F, A],
        Primitive.Required.Reader[F, A],
        Primitive.Required.Writer[F, A]:
    final override def imap[C](f: A => C)(g: C => A): Primitive.Required[F, C] = transform(Transformation.lift(f)(g))
    override def transform[B, C, D](transformation: PrimitiveTransformation[F, A, B, C, D]): Primitive.Required[F, D] =
      Required.Transform(this, transformation)

  object Required:
    sealed trait Reader[F[+_], +A] extends Primitive.Reader[F, A]:
      final override def map[C](f: A => C): Primitive.Required.Reader[F, C] = transform(Transformation.Reader.lift(f))
      final override def transform[A1 >: A, B, C, D](
          transformation: PrimitiveTransformation.Reader[F, A1, B, C, D]
      ): Primitive.Required.Reader[F, D] = Reader.Transform(this, transformation)

    object Reader:
      final case class Transform[F[+_], A, B, C, D](
          self: Primitive.Required.Reader[F, A],
          transformation: PrimitiveTransformation.Reader[F, A, B, C, D]
      ) extends Primitive.Required.Reader[F, D]:
        export self.tpe
        override def constraints: Chain[Constraint.Primitive[?]] =
          self.constraints ++ transformation.validation.constraints

    sealed trait Writer[F[+_], -A] extends Primitive.Writer[F, A]:
      final override def contramap[B](f: B => A): Primitive.Required.Writer[F, B] =
        transform(Transformation.Writer(f))
      final override def transform[A1 <: A, B](
          transformation: Transformation.Writer[A1, B]
      ): Primitive.Required.Writer[F, B] = Writer.Transform(this, transformation)

    object Writer:
      final case class Transform[F[+_], A, B](
          self: Primitive.Required.Writer[F, A],
          transformation: Transformation.Writer[A, B]
      ) extends Primitive.Required.Writer[F, B]:
        export self.tpe

    final case class Root[F[+_], A](tpe: Type[A]) extends Primitive.Required[F, A]:
      override def constraints: Chain[Constraint.Primitive[?]] = Chain.empty

    final case class Transform[F[+_], A, B, C, D](
        self: Primitive.Required[F, A],
        transformation: PrimitiveTransformation[F, A, B, C, D]
    ) extends Primitive.Required[F, D]:
      export self.tpe
      override def constraints: Chain[Constraint.Primitive[?]] =
        self.constraints ++ transformation.validation.constraints

  sealed trait Reader[F[+_], +A] extends Schema.Reader[F, Nothing, A]:
    def constraints: Chain[Constraint.Primitive[?]]
    override def map[C](f: A => C): Primitive.Reader[F, C] = transform(Transformation.Reader.lift(f))
    override def optional: Primitive.Reader[F, Option[A]] = Reader.Optional(this)
    def tpe: Type[?]
    def transform[A1 >: A, B, C, D](
        transformation: PrimitiveTransformation.Reader[F, A1, B, C, D]
    ): Primitive.Reader[F, D] = Reader.Transform(this, transformation)

  object Reader:
    final case class Transform[F[+_], A, B, C, D](
        self: Primitive.Reader[F, A],
        transformation: PrimitiveTransformation.Reader[F, A, B, C, D]
    ) extends Primitive.Reader[F, D]:
      export self.tpe
      override def constraints: Chain[Constraint.Primitive[?]] =
        self.constraints ++ transformation.validation.constraints

    final case class Optional[F[+_], A](self: Primitive.Reader[F, A]) extends Primitive.Reader[F, Option[A]]:
      export self.{constraints, tpe}

  sealed trait Writer[F[+_], -A] extends Schema.Writer[F, Nothing, A]:
    override def contramap[B](f: B => A): Primitive.Writer[F, B] = transform(Transformation.Writer(f))
    override def optional: Primitive.Writer[F, Option[A]] = Writer.Optional(this)
    def tpe: Type[?]
    def transform[A1 <: A, B](transformation: Transformation.Writer[A1, B]): Primitive.Writer[F, B] =
      Writer.Transform(this, transformation)

  object Writer:
    final case class Transform[F[+_], A, B](self: Primitive.Writer[F, A], transformation: Transformation.Writer[A, B])
        extends Primitive.Writer[F, B]:
      export self.tpe

    final case class Optional[F[+_], A](self: Primitive.Writer[F, A]) extends Primitive.Writer[F, Option[A]]:
      export self.tpe

  final case class Optional[F[+_], A](self: Primitive[F, A]) extends Primitive[F, Option[A]]:
    export self.{constraints, tpe}

  final case class Transform[F[+_], A, B, C, D](
      self: Primitive[F, A],
      transformation: PrimitiveTransformation[F, A, B, C, D]
  ) extends Primitive[F, D]:
    export self.tpe
    override def constraints: Chain[Constraint.Primitive[?]] = self.constraints ++ transformation.validation.constraints

sealed trait Union[F[+_], +A, B] extends Schema[F, A, B], Union.Reader[F, A, B], Union.Writer[F, A, B]:
  override def imap[C](f: B => C)(g: C => B): Union[F, A, C] = ???
  override def optional: Union[F, A, Option[B]] = ???

object Union:
  sealed trait Reader[F[+_], +A, +B] extends Schema.Reader[F, A, B]:
    final override def map[C](f: B => C): Union.Reader[F, A, C] = ???
    override def optional: Union.Reader[F, A, Option[B]] = ???

  object Reader:
    final case class OrElse[F[+_], A, B, C, D](left: Union.Reader[F, A, B], right: Union.Reader[F, A, B])
        extends Union.Reader[F, A | C, Either[B, D]]

    final case class Root[F[+_], A <: F[Schema.Reader[F, ?, B]], B](schema: A) extends Union.Reader[F, A, B]

  sealed trait Writer[F[+_], +A, -B] extends Schema.Writer[F, A, B]:
    final override def contramap[C](f: C => B): Union.Writer[F, A, C] = ???
    override def optional: Union.Writer[F, A, Option[B]] = ???

  object Writer:
    final case class OrElse[F[+_], A, B, C, D](left: Union.Writer[F, A, B], right: Union.Writer[F, A, B])
        extends Union.Writer[F, A | C, Either[B, D]]

    final case class Root[F[+_], A <: F[Schema.Writer[F, ?, B]], B](schema: A) extends Union.Writer[F, A, B]

  final case class OrElse[F[+_], A, B, C, D](left: Union[F, A, B], right: Union[F, C, D])
      extends Union[F, A | C, Either[B, D]]

  final case class Root[F[+_], A <: F[Schema[F, ?, B]], B](schema: A) extends Union[F, A, B]
