package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Schema.Reader
import io.taig.otter.validation.Validation
import cats.data.Chain

sealed trait Schema[+M, +A, B] extends Schema.Reader[M, A, B], Schema.Writer[M, A, B]:
  def asReader: Schema.Reader[M, A, B] = this
  def asWriter: Schema.Writer[M, A, B] = this
  final override def collectionWith[N](metadata: N): Collection[N, this.type, Vector[B]] =
    Collection.Root(metadata, this)
  def imap[C](f: B => C)(g: C => B): Schema[M, A, C]
  override def mapMetadata[N](f: M => N): Schema[N, A, B]
  override def optional: Schema[M, A, Option[B]]
  final override def unionWith[N](metadata: N): Union[N, this.type, B] = Union.Root(metadata, this)

object Schema:
  sealed trait Reader[+M, +A, +B] extends Product, Serializable:
    def collectionWith[N](metadata: N): Collection.Reader[N, this.type, Vector[B]] =
      Collection.Reader.Root(metadata, this)
    def map[C](f: B => C): Schema.Reader[M, A, C]
    def mapMetadata[N](f: M => N): Schema.Reader[N, A, B]
    def metadata: M
    def optional: Schema.Reader[M, A, Option[B]]
    def unionWith[N](metadata: N): Union.Reader[N, this.type, B] = Union.Reader.Root(metadata, this)

  sealed trait Writer[+M, +A, -B] extends Product, Serializable:
    def collectionWith[N](metadata: N): Collection.Writer[N, this.type, Vector[B]] =
      Collection.Writer.Root(metadata, this)
    def contramap[C](f: C => B): Schema.Writer[M, A, C]
    def mapMetadata[N](f: M => N): Schema.Writer[N, A, B]
    def metadata: M
    def optional: Schema.Writer[M, A, Option[B]]
    def unionWith[N](metadata: N): Union.Writer[N, this.type, B] = Union.Writer.Root(metadata, this)

sealed trait Collection[+M, +A, B] extends Schema[M, A, B], Collection.Reader[M, A, B], Collection.Writer[M, A, B]:
  final override def imap[C](f: B => C)(g: C => B): Collection[M, A, C] = ivalidate(Validation.lift(f))(g)
  final def ivalidate[N >: M, C, D](
      validation: Validation[B, Constraint.Collection, (Schema.Writer[N, ?, C], C), D]
  )(f: D => B): Collection[M, A, D] = Collection.Validate(this, validation, f)
  override def mapMetadata[N](f: M => N): Collection[N, A, B]
  final override def optional: Collection[M, A, Option[B]] = Collection.Optional(this)
  override def schema: Schema[?, ?, ?]

object Collection:
  sealed trait Reader[+M, +A, +B] extends Schema.Reader[M, A, B]:
    def constraints: Chain[Constraint.Collection]
    final override def map[C](f: B => C): Collection.Reader[M, A, C] = validate(Validation.lift(f))
    override def mapMetadata[N](f: M => N): Collection.Reader[N, A, B]
    override def optional: Collection.Reader[M, A, Option[B]] = Collection.Reader.Optional(this)
    def schema: Schema.Reader[?, ?, ?]
    final def validate[N >: M, C, D](
        validation: Validation[B, Constraint.Collection, (Schema.Writer[N, ?, C], C), D]
    ): Collection.Reader[M, A, D] = Reader.Validate(this, validation)

  object Reader:
    final case class Validate[M, N >: M, A, B, C, D](
        self: Collection.Reader[M, A, B],
        validation: Validation[B, Constraint.Collection, (Schema.Writer[N, ?, C], C), D]
    ) extends Collection.Reader[M, A, D]:
      export self.{metadata, schema}
      override def constraints: Chain[Constraint.Collection] = self.constraints ++ validation.constraints
      override def mapMetadata[N](f: M => N): Collection.Reader[N, A, D] = copy(self = self.mapMetadata(f))

    final case class Optional[M, A, B](self: Collection.Reader[M, A, B]) extends Collection.Reader[M, A, Option[B]]:
      export self.{constraints, metadata, schema}
      override def mapMetadata[N](f: M => N): Collection.Reader[N, A, Option[B]] = copy(self = self.mapMetadata(f))

    final case class Root[M, A <: Schema.Reader[?, ?, B], B](metadata: M, schema: A)
        extends Collection.Reader[M, A, Vector[B]]:
      override def constraints: Chain[Constraint.Collection] = Chain.empty
      override def mapMetadata[N](f: M => N): Collection.Reader[N, A, Vector[B]] = copy(metadata = f(metadata))

  sealed trait Writer[+M, +A, -B] extends Schema.Writer[M, A, B]:
    final def contramap[C](f: C => B): Collection.Writer[M, A, C] = Writer.Modify(this, f)
    override def mapMetadata[N](f: M => N): Collection.Writer[N, A, B]
    def optional: Collection.Writer[M, A, Option[B]] = Writer.Optional(this)
    def schema: Schema.Writer[?, ?, ?]

  object Writer:
    final case class Modify[M, A, B, C](
        self: Collection.Writer[M, A, B],
        f: C => B
    ) extends Collection.Writer[M, A, C]:
      export self.{metadata, schema}
      override def mapMetadata[N](f: M => N): Collection.Writer[N, A, C] = copy(self = self.mapMetadata(f))

    final case class Optional[M, A, B](self: Collection.Writer[M, A, B]) extends Collection.Writer[M, A, Option[B]]:
      export self.{metadata, schema}
      override def mapMetadata[N](f: M => N): Collection.Writer[N, A, Option[B]] = copy(self = self.mapMetadata(f))

    final case class Root[M, A <: Schema.Writer[?, ?, B], B](metadata: M, schema: A)
        extends Collection.Writer[M, A, Vector[B]]:
      override def mapMetadata[N](f: M => N): Collection.Writer[N, A, Vector[B]] = copy(metadata = f(metadata))

  final case class Optional[M, A, B](self: Collection[M, A, B]) extends Collection[M, A, Option[B]]:
    export self.{constraints, metadata, schema}
    override def mapMetadata[N](f: M => N): Collection[N, A, Option[B]] = copy(self = self.mapMetadata(f))

  final case class Root[M, A <: Schema[?, ?, B], B](metadata: M, schema: A) extends Collection[M, A, Vector[B]]:
    override def constraints: Chain[Constraint.Collection] = Chain.empty
    override def mapMetadata[N](f: M => N): Collection[N, A, Vector[B]] = copy(metadata = f(metadata))

  final case class Validate[M, N >: M, A, B, C, D](
      self: Collection[M, A, B],
      validation: Validation[B, Constraint.Collection, (Schema.Writer[N, ?, C], C), D],
      f: D => B
  ) extends Collection[M, A, D]:
    export self.{metadata, schema}
    override def constraints: Chain[Constraint.Collection] = self.constraints ++ validation.constraints
    override def mapMetadata[N](f: M => N): Collection[N, A, D] = copy(self = self.mapMetadata(f))

sealed trait Primitive[+M, A] extends Schema[M, Nothing, A], Primitive.Reader[M, A], Primitive.Writer[M, A]:
  override def imap[C](f: A => C)(g: C => A): Primitive[M, C] = ivalidate(Validation.lift(f))(g)
  def ivalidate[N >: M, B, C, D](
      validation: Validation[A, Constraint.Primitive[(Schema.Writer[N, ?, B], B)], (Schema.Writer[N, ?, C], C), D]
  )(f: D => A): Primitive[M, D] = Primitive.Validate(this, validation, f)
  override def mapMetadata[N](f: M => N): Primitive[N, A]
  final override def optional: Primitive[M, Option[A]] = Primitive.Optional(this)

object Primitive:
  sealed trait Required[+M, A]
      extends Primitive[M, A],
        Primitive.Required.Reader[M, A],
        Primitive.Required.Writer[M, A]:
    final override def imap[C](f: A => C)(g: C => A): Primitive.Required[M, C] = ivalidate(Validation.lift(f))(g)
    final override def ivalidate[N >: M, B, C, D](
        validation: Validation[A, Constraint.Primitive[(Schema.Writer[N, ?, B], B)], (Schema.Writer[N, ?, C], C), D]
    )(f: D => A): Primitive.Required[M, D] = Primitive.Required.Validate(this, validation, f)
    override def mapMetadata[N](f: M => N): Primitive.Required[N, A]

  object Required:
    sealed trait Reader[+M, +A] extends Primitive.Reader[M, A]:
      override def map[C](f: A => C): Primitive.Required.Reader[M, C] = validate(Validation.lift(f))
      override def mapMetadata[N](f: M => N): Primitive.Required.Reader[N, A]
      final override def validate[N >: M, B, C, D](
          validation: Validation[A, Constraint.Primitive[(Schema.Writer[N, ?, B], B)], (Schema.Writer[N, ?, C], C), D]
      ): Primitive.Required.Reader[M, D] = Reader.Validate(this, validation)

    object Reader:
      final case class Validate[M, N >: M, A, B, C, D](
          self: Primitive.Required.Reader[M, A],
          validation: Validation[A, Constraint.Primitive[(Schema.Writer[N, ?, B], B)], (Schema.Writer[N, ?, C], C), D]
      ) extends Primitive.Required.Reader[M, D]:
        export self.{metadata, tpe}
        override def constraints: Chain[Constraint.Primitive[?]] = self.constraints ++ validation.constraints
        override def mapMetadata[N](f: M => N): Primitive.Required.Reader[N, D] = copy(self = self.mapMetadata(f))

    sealed trait Writer[+M, -A] extends Primitive.Writer[M, A]:
      final override def contramap[B](f: B => A): Primitive.Required.Writer[M, B] = Writer.Modify(this, f)
      override def mapMetadata[N](f: M => N): Primitive.Required.Writer[N, A]

    object Writer:
      final case class Modify[M, A, B](self: Primitive.Required.Writer[M, A], f: B => A)
          extends Primitive.Required.Writer[M, B]:
        export self.{metadata, tpe}
        override def mapMetadata[N](f: M => N): Primitive.Required.Writer[N, B] = copy(self = self.mapMetadata(f))

    final case class Root[M, A](metadata: M, tpe: Type[A]) extends Primitive.Required[M, A]:
      override def constraints: Chain[Constraint.Primitive[?]] = Chain.empty
      override def mapMetadata[N](f: M => N): Primitive.Required[N, A] = copy(metadata = f(metadata))

    final case class Validate[M, N >: M, A, B, C, D](
        self: Primitive.Required[M, A],
        validation: Validation[A, Constraint.Primitive[(Schema.Writer[N, ?, B], B)], (Schema.Writer[N, ?, C], C), D],
        f: D => A
    ) extends Primitive.Required[M, D]:
      export self.{metadata, tpe}
      override def constraints: Chain[Constraint.Primitive[?]] = self.constraints ++ validation.constraints
      override def mapMetadata[N](f: M => N): Primitive.Required[N, D] = copy(self = self.mapMetadata(f))

  sealed trait Reader[+M, +A] extends Schema.Reader[M, Nothing, A]:
    def constraints: Chain[Constraint.Primitive[?]]
    override def map[C](f: A => C): Primitive.Reader[M, C] = validate(Validation.lift(f))
    override def mapMetadata[N](f: M => N): Primitive.Reader[N, A]
    override def optional: Primitive.Reader[M, Option[A]] = Reader.Optional(this)
    def tpe: Type[?]
    def validate[N >: M, B, C, D](
        validation: Validation[A, Constraint.Primitive[(Schema.Writer[N, ?, B], B)], (Schema.Writer[N, ?, C], C), D]
    ): Primitive.Reader[M, D] = Reader.Validate(this, validation)

  object Reader:
    final case class Validate[M, N >: M, A, B, C, D](
        self: Primitive.Reader[M, A],
        validation: Validation[A, Constraint.Primitive[(Schema.Writer[N, ?, B], B)], (Schema.Writer[N, ?, C], C), D]
    ) extends Primitive.Reader[M, D]:
      export self.{metadata, tpe}
      override def constraints: Chain[Constraint.Primitive[?]] = self.constraints ++ validation.constraints
      override def mapMetadata[N](f: M => N): Primitive.Reader[N, D] = copy(self = self.mapMetadata(f))

    final case class Optional[M, A](self: Primitive.Reader[M, A]) extends Primitive.Reader[M, Option[A]]:
      export self.{constraints, metadata, tpe}
      override def mapMetadata[N](f: M => N): Primitive.Reader[N, Option[A]] = copy(self = self.mapMetadata(f))

  sealed trait Writer[+M, -A] extends Schema.Writer[M, Nothing, A]:
    def contramap[C](f: C => A): Primitive.Writer[M, C] = Writer.Modify(this, f)
    override def mapMetadata[N](f: M => N): Primitive.Writer[N, A]
    def optional: Primitive.Writer[M, Option[A]] = Writer.Optional(this)
    def tpe: Type[?]

  object Writer:
    final case class Modify[M, A, B](self: Primitive.Writer[M, A], f: B => A) extends Primitive.Writer[M, B]:
      export self.{metadata, tpe}
      override def mapMetadata[N](f: M => N): Primitive.Writer[N, B] = copy(self = self.mapMetadata(f))

    final case class Optional[M, A](self: Primitive.Writer[M, A]) extends Primitive.Writer[M, Option[A]]:
      export self.{metadata, tpe}
      override def mapMetadata[N](f: M => N): Primitive.Writer[N, Option[A]] = copy(self = self.mapMetadata(f))

  final case class Optional[M, A](self: Primitive[M, A]) extends Primitive[M, Option[A]]:
    export self.{constraints, metadata, tpe}
    override def mapMetadata[N](f: M => N): Primitive[N, Option[A]] = copy(self = self.mapMetadata(f))

  final case class Validate[M, N >: M, A, B, C, D](
      self: Primitive[M, A],
      validation: Validation[A, Constraint.Primitive[(Schema.Writer[N, ?, B], B)], (Schema.Writer[N, ?, C], C), D],
      f: D => A
  ) extends Primitive[M, D]:
    export self.{metadata, tpe}
    override def constraints: Chain[Constraint.Primitive[?]] = self.constraints ++ validation.constraints
    override def mapMetadata[N](f: M => N): Primitive[N, D] = copy(self = self.mapMetadata(f))

sealed trait Union[+M, +A, B] extends Schema[M, A, B], Union.Reader[M, A, B], Union.Writer[M, A, B]:
  override def imap[C](f: B => C)(g: C => B): Union[M, A, C] = ???
  override def mapMetadata[N](f: M => N): Union[N, A, B]
  override def optional: Union[M, A, Option[B]] = ???

object Union:
  sealed trait Reader[+M, +A, +B] extends Schema.Reader[M, A, B]:
    final override def map[C](f: B => C): Union.Reader[M, A, C] = ???
    override def optional: Union.Reader[M, A, Option[B]] = ???
    override def mapMetadata[N](f: M => N): Union.Reader[N, A, B]

  object Reader:
    final case class Root[M, A <: Schema.Reader[?, ?, B], B](metadata: M, schema: A) extends Union.Reader[M, A, B]:
      override def mapMetadata[N](f: M => N): Union.Reader[N, A, B] = copy(metadata = f(metadata))

  sealed trait Writer[+M, +A, -B] extends Schema.Writer[M, A, B]:
    final override def contramap[C](f: C => B): Union.Writer[M, A, C] = ???
    override def mapMetadata[N](f: M => N): Union.Writer[N, A, B]
    override def optional: Union.Writer[M, A, Option[B]] = ???

  object Writer:
    final case class Root[M, A <: Schema.Writer[?, ?, B], B](metadata: M, schema: A) extends Union.Writer[M, A, B]:
      override def mapMetadata[N](f: M => N): Union.Writer[N, A, B] = copy(metadata = f(metadata))

  final case class Root[M, A <: Schema[?, ?, B], B](metadata: M, schema: A) extends Union[M, A, B]:
    override def mapMetadata[N](f: M => N): Union[N, A, B] = copy(metadata = f(metadata))
