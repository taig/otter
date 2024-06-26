package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Schema.Reader
import io.taig.otter.validation.Validation
import cats.data.Chain
import cats.Invariant
import cats.Functor

sealed trait Schema[M, +N <: M, +A, B] extends Schema.Reader[M, N, A, B], Schema.Writer[M, N, A, B]:
  def asReader: Schema.Reader[M, N, A, B] = this
  def asWriter: Schema.Writer[M, N, A, B] = this
  final override def collectionWith[N1 <: M](metadata: N1): Collection[M, N1, this.type, Vector[B]] =
    Collection.Root(metadata, this)
  def imap[C](f: B => C)(g: C => B): Schema[M, N, A, C]
  override def optional: Schema[M, N, A, Option[B]]
  final override def unionWith[N1 <: M](metadata: N1): Union[M, N1, this.type, B] = Union.Root(metadata, this)

object Schema:
  sealed trait Reader[M, +N <: M, +A, +B] extends Product, Serializable:
    def collectionWith[N1 <: M](metadata: N1): Collection.Reader[M, N1, this.type, Vector[B]] =
      Collection.Reader.Root(metadata, this)
    def map[C](f: B => C): Schema.Reader[M, N, A, C]
    def metadata: N
    def optional: Schema.Reader[M, N, A, Option[B]]
    def unionWith[N1 <: M](metadata: N1): Union.Reader[M, N1, this.type, B] = Union.Reader.Root(metadata, this)

  object Reader:
    given [M, N <: M, A]: Functor[Schema.Reader[M, N, A, *]] with
      override def map[B, C](fa: Schema.Reader[M, N, A, B])(f: B => C): Schema.Reader[M, N, A, C] = fa.map(f)

  sealed trait Writer[M, +N <: M, +A, -B] extends Product, Serializable:
    def collectionWith[N1 <: M](metadata: N1): Collection.Writer[M, N1, this.type, Vector[B]] =
      Collection.Writer.Root(metadata, this)
    def contramap[C](f: C => B): Schema.Writer[M, N, A, C]
    def metadata: N
    def optional: Schema.Writer[M, N, A, Option[B]]
    def unionWith[N1 <: M](metadata: N1): Union.Writer[M, N1, this.type, B] = Union.Writer.Root(metadata, this)

  given [M, N <: M, A]: Invariant[Schema[M, N, A, *]] with
    override def imap[B, C](fa: Schema[M, N, A, B])(f: B => C)(g: C => B): Schema[M, N, A, C] = fa.imap(f)(g)

sealed trait Collection[M, +N <: M, +A, B]
    extends Schema[M, N, A, B],
      Collection.Reader[M, N, A, B],
      Collection.Writer[M, N, A, B]:
  final override def imap[C](f: B => C)(g: C => B): Collection[M, N, A, C] = ivalidate(Validation.lift(f))(g)
  final def ivalidate[C, D](
      validation: Validation[B, Constraint.Collection, (Schema.Writer[M, ?, ?, C], C), D]
  )(f: D => B): Collection[M, N, A, D] = Collection.Validate(this, validation, f)
  final override def optional: Collection[M, N, A, Option[B]] = Collection.Optional(this)
  override def schema: Schema[M, ?, ?, ?]

object Collection:
  sealed trait Reader[M, +N <: M, +A, +B] extends Schema.Reader[M, N, A, B]:
    def constraints: Chain[Constraint.Collection]
    final override def map[C](f: B => C): Collection.Reader[M, N, A, C] = validate(Validation.lift(f))
    override def optional: Collection.Reader[M, N, A, Option[B]] = Collection.Reader.Optional(this)
    def schema: Schema.Reader[M, ?, ?, ?]
    final def validate[C, D](
        validation: Validation[B, Constraint.Collection, (Schema.Writer[M, ?, ?, C], C), D]
    ): Collection.Reader[M, N, A, D] = Reader.Validate(this, validation)

  object Reader:
    final case class Validate[M, N <: M, A, B, C, D](
        self: Collection.Reader[M, N, A, B],
        validation: Validation[B, Constraint.Collection, (Schema.Writer[M, ?, ?, C], C), D]
    ) extends Collection.Reader[M, N, A, D]:
      export self.{metadata, schema}
      override def constraints: Chain[Constraint.Collection] = self.constraints ++ validation.constraints

    final case class Optional[M, N <: M, A, B](self: Collection.Reader[M, N, A, B])
        extends Collection.Reader[M, N, A, Option[B]]:
      export self.{constraints, metadata, schema}

    final case class Root[M, N <: M, A <: Schema.Reader[M, ?, ?, B], B](metadata: N, schema: A)
        extends Collection.Reader[M, N, A, Vector[B]]:
      override def constraints: Chain[Constraint.Collection] = Chain.empty

  sealed trait Writer[M, +N <: M, +A, -B] extends Schema.Writer[M, N, A, B]:
    final def contramap[C](f: C => B): Collection.Writer[M, N, A, C] = Writer.Modify(this, f)
    def optional: Collection.Writer[M, N, A, Option[B]] = Writer.Optional(this)
    def schema: Schema.Writer[M, ?, ?, ?]

  object Writer:
    final case class Modify[M, N <: M, A, B, C](
        self: Collection.Writer[M, N, A, B],
        f: C => B
    ) extends Collection.Writer[M, N, A, C]:
      export self.{metadata, schema}

    final case class Optional[M, N <: M, A, B](self: Collection.Writer[M, N, A, B])
        extends Collection.Writer[M, N, A, Option[B]]:
      export self.{metadata, schema}

    final case class Root[M, N <: M, A <: Schema.Writer[M, ?, ?, B], B](metadata: N, schema: A)
        extends Collection.Writer[M, N, A, Vector[B]]

  final case class Optional[M, N <: M, A, B](self: Collection[M, N, A, B]) extends Collection[M, N, A, Option[B]]:
    export self.{constraints, metadata, schema}

  final case class Root[M, N <: M, A <: Schema[M, ?, ?, B], B](metadata: N, schema: A)
      extends Collection[M, N, A, Vector[B]]:
    override def constraints: Chain[Constraint.Collection] = Chain.empty

  final case class Validate[M, N <: M, A, B, C, D](
      self: Collection[M, N, A, B],
      validation: Validation[B, Constraint.Collection, (Schema.Writer[M, ?, ?, C], C), D],
      f: D => B
  ) extends Collection[M, N, A, D]:
    export self.{metadata, schema}
    override def constraints: Chain[Constraint.Collection] = self.constraints ++ validation.constraints

sealed trait Primitive[M, +N <: M, A]
    extends Schema[M, N, Nothing, A],
      Primitive.Reader[M, N, A],
      Primitive.Writer[M, N, A]:
  override def imap[C](f: A => C)(g: C => A): Primitive[M, N, C] = ivalidate(Validation.lift(f))(g)
  def ivalidate[B, C, D](
      validation: Validation[A, Constraint.Primitive[(Schema.Writer[M, ?, ?, B], B)], (Schema.Writer[M, ?, ?, C], C), D]
  )(f: D => A): Primitive[M, N, D] = Primitive.Validate(this, validation, f)
  final override def optional: Primitive[M, N, Option[A]] = Primitive.Optional(this)

object Primitive:
  sealed trait Required[M, +N <: M, A]
      extends Primitive[M, N, A],
        Primitive.Required.Reader[M, N, A],
        Primitive.Required.Writer[M, N, A]:
    final override def imap[C](f: A => C)(g: C => A): Primitive.Required[M, N, C] = ivalidate(Validation.lift(f))(g)
    final override def ivalidate[B, C, D](
        validation: Validation[A, Constraint.Primitive[
          (Schema.Writer[M, ?, ?, B], B)
        ], (Schema.Writer[M, ?, ?, C], C), D]
    )(f: D => A): Primitive.Required[M, N, D] = Primitive.Required.Validate(this, validation, f)

  object Required:
    sealed trait Reader[M, +N <: M, +A] extends Primitive.Reader[M, N, A]:
      override def map[C](f: A => C): Primitive.Required.Reader[M, N, C] = validate(Validation.lift(f))
      final override def validate[B, C, D](
          validation: Validation[A, Constraint.Primitive[
            (Schema.Writer[M, ?, ?, B], B)
          ], (Schema.Writer[M, ?, ?, C], C), D]
      ): Primitive.Required.Reader[M, N, D] = Reader.Validate(this, validation)

    object Reader:
      final case class Validate[M, N <: M, A, B, C, D](
          self: Primitive.Required.Reader[M, N, A],
          validation: Validation[A, Constraint.Primitive[
            (Schema.Writer[M, ?, ?, B], B)
          ], (Schema.Writer[M, ?, ?, C], C), D]
      ) extends Primitive.Required.Reader[M, N, D]:
        export self.{metadata, tpe}
        override def constraints: Chain[Constraint.Primitive[?]] = self.constraints ++ validation.constraints

    sealed trait Writer[M, +N <: M, -A] extends Primitive.Writer[M, N, A]:
      final override def contramap[B](f: B => A): Primitive.Required.Writer[M, N, B] = Writer.Modify(this, f)

    object Writer:
      final case class Modify[M, N <: M, A, B](self: Primitive.Required.Writer[M, N, A], f: B => A)
          extends Primitive.Required.Writer[M, N, B]:
        export self.{metadata, tpe}

    final case class Root[M, N <: M, A](metadata: N, tpe: Type[A]) extends Primitive.Required[M, N, A]:
      override def constraints: Chain[Constraint.Primitive[?]] = Chain.empty

    final case class Validate[M, N <: M, A, B, C, D](
        self: Primitive.Required[M, N, A],
        validation: Validation[A, Constraint.Primitive[
          (Schema.Writer[M, ?, ?, B], B)
        ], (Schema.Writer[M, ?, ?, C], C), D],
        f: D => A
    ) extends Primitive.Required[M, N, D]:
      export self.{metadata, tpe}
      override def constraints: Chain[Constraint.Primitive[?]] = self.constraints ++ validation.constraints

  sealed trait Reader[M, +N <: M, +A] extends Schema.Reader[M, N, Nothing, A]:
    def constraints: Chain[Constraint.Primitive[?]]
    override def map[C](f: A => C): Primitive.Reader[M, N, C] = validate(Validation.lift(f))
    override def optional: Primitive.Reader[M, N, Option[A]] = Reader.Optional(this)
    def tpe: Type[?]
    def validate[B, C, D](
        validation: Validation[A, Constraint.Primitive[
          (Schema.Writer[M, ?, ?, B], B)
        ], (Schema.Writer[M, ?, ?, C], C), D]
    ): Primitive.Reader[M, N, D] = Reader.Validate(this, validation)

  object Reader:
    final case class Validate[M, N <: M, A, B, C, D](
        self: Primitive.Reader[M, N, A],
        validation: Validation[A, Constraint.Primitive[
          (Schema.Writer[M, ?, ?, B], B)
        ], (Schema.Writer[M, ?, ?, C], C), D]
    ) extends Primitive.Reader[M, N, D]:
      export self.{metadata, tpe}
      override def constraints: Chain[Constraint.Primitive[?]] = self.constraints ++ validation.constraints

    final case class Optional[M, N <: M, A](self: Primitive.Reader[M, N, A]) extends Primitive.Reader[M, N, Option[A]]:
      export self.{constraints, metadata, tpe}

  sealed trait Writer[M, +N <: M, -A] extends Schema.Writer[M, N, Nothing, A]:
    def contramap[C](f: C => A): Primitive.Writer[M, N, C] = Writer.Modify(this, f)
    def optional: Primitive.Writer[M, N, Option[A]] = Writer.Optional(this)
    def tpe: Type[?]

  object Writer:
    final case class Modify[M, N <: M, A, B](self: Primitive.Writer[M, N, A], f: B => A)
        extends Primitive.Writer[M, N, B]:
      export self.{metadata, tpe}

    final case class Optional[M, N <: M, A](self: Primitive.Writer[M, N, A]) extends Primitive.Writer[M, N, Option[A]]:
      export self.{metadata, tpe}

  final case class Optional[M, N <: M, A](self: Primitive[M, N, A]) extends Primitive[M, N, Option[A]]:
    export self.{constraints, metadata, tpe}

  final case class Validate[M, N <: M, A, B, C, D](
      self: Primitive[M, N, A],
      validation: Validation[A, Constraint.Primitive[
        (Schema.Writer[M, ?, ?, B], B)
      ], (Schema.Writer[M, ?, ?, C], C), D],
      f: D => A
  ) extends Primitive[M, N, D]:
    export self.{metadata, tpe}
    override def constraints: Chain[Constraint.Primitive[?]] = self.constraints ++ validation.constraints

sealed trait Union[M, +N <: M, +A, B] extends Schema[M, N, A, B], Union.Reader[M, N, A, B], Union.Writer[M, N, A, B]:
  override def imap[C](f: B => C)(g: C => B): Union[M, N, A, C] = ???
  override def optional: Union[M, N, A, Option[B]] = ???

object Union:
  sealed trait Reader[M, +N <: M, +A, +B] extends Schema.Reader[M, N, A, B]:
    final override def map[C](f: B => C): Union.Reader[M, N, A, C] = ???
    override def optional: Union.Reader[M, N, A, Option[B]] = ???

  object Reader:
    final case class Root[M, N <: M, A <: Schema.Reader[M, ?, ?, B], B](metadata: N, schema: A)
        extends Union.Reader[M, N, A, B]

  sealed trait Writer[M, +N <: M, +A, -B] extends Schema.Writer[M, N, A, B]:
    final override def contramap[C](f: C => B): Union.Writer[M, N, A, C] = ???
    override def optional: Union.Writer[M, N, A, Option[B]] = ???

  object Writer:
    final case class OrElse[M, N <: M, A, B, C, D](
        metadata: N,
        left: Union.Writer[M, N, A, B],
        right: Union.Writer[M, N, A, B]
    ) extends Union.Writer[M, N, A | C, Either[B, D]]

    final case class Root[M, N <: M, A <: Schema.Writer[M, ?, ?, B], B](metadata: N, schema: A)
        extends Union.Writer[M, N, A, B]

  final case class OrElse[M, N <: M, A, B, C, D](metadata: N, left: Union[M, N, A, B], right: Union[M, N, C, D])
      extends Union[M, N, A | C, Either[B, D]]

  final case class Root[M, N <: M, A <: Schema[M, ?, ?, B], B](metadata: N, schema: A) extends Union[M, N, A, B]
