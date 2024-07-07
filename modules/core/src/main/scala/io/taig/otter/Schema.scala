package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Schema.Reader
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

sealed trait Schema[+F[+_], +A, B] extends Schema.Reader[F, A, B], Schema.Writer[F, A, B]:
  override def default: Option[B] = ???
  override def default[B1 >: B](value: B1): Schema[F, A, B1] = ???

  def imap[C](f: B => C)(g: C => B): Schema[F, A, C]
  override def optional: Schema[F, A, Option[B]]
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Schema[G, ?, B]

object Schema:
  sealed trait Reader[+F[+_], +A, +B] extends SProduct, Serializable:
    def default: Option[B] = ???
    def default[B1 >: B](value: B1): Schema.Reader[F, A, B1] = ???

    def map[C](f: B => C): Schema.Reader[F, A, C]
    def optional: Schema.Reader[F, A, Option[B]]
    def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Schema.Reader[G, ?, B]

  sealed trait Writer[+F[+_], +A, -B] extends SProduct, Serializable:
    def contramap[C](f: C => B): Schema.Writer[F, A, C]
    def optional: Schema.Writer[F, A, Option[B]]
    def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Schema.Writer[G, ?, B]

sealed trait Value[+F[+_], +A, B] extends Schema[F, A, B], Value.Reader[F, A, B], Value.Writer[F, A, B]:
  override def imap[C](f: B => C)(g: C => B): Value[F, A, C]
  override def optional: Value[F, A, Option[B]]
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Value[G, ?, B]

object Value:
  sealed trait Required[+F[+_], +A, B]
      extends Value[F, A, B],
        Value.Required.Reader[F, A, B],
        Value.Required.Writer[F, A, B]:
    override def imap[C](f: B => C)(g: C => B): Value.Required[F, A, C]
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Value.Required[G, ?, B]

  object Required:
    sealed trait Reader[+F[+_], +A, +B] extends Value.Reader[F, A, B]:
      override def map[C](f: B => C): Value.Required.Reader[F, A, C]
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Value.Required.Reader[G, ?, B]

    sealed trait Writer[+F[+_], +A, -B] extends Value.Writer[F, A, B]:
      override def contramap[C](f: C => B): Value.Required.Writer[F, A, C]
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Value.Required.Writer[G, ?, B]

  sealed trait Reader[+F[+_], +A, +B] extends Schema.Reader[F, A, B]:
    override def map[C](f: B => C): Value.Reader[F, A, C]
    override def optional: Value.Reader[F, A, Option[B]]
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Value.Reader[G, ?, B]

  sealed trait Writer[+F[+_], +A, -B] extends Schema.Writer[F, A, B]:
    override def contramap[C](f: C => B): Value.Writer[F, A, C]
    override def optional: Value.Writer[F, A, Option[B]]
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Value.Writer[G, ?, B]

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

sealed trait Dictionary[+F[+_], +A, B] extends Schema[F, A, B], Dictionary.Reader[F, A, B], Dictionary.Writer[F, A, B]:
  override def imap[C](f: B => C)(g: C => B): Dictionary[F, A, C] = Dictionary.Transform(this, f, g)
  override def optional: Dictionary[F, A, Option[B]] = Dictionary.Optional(this)
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Dictionary[G, ?, B]

object Dictionary:
  sealed trait Reader[+F[+_], +A, +B] extends Schema.Reader[F, A, B]:
    override def map[C](f: B => C): Dictionary.Reader[F, A, C] = Reader.Transform(this, f)
    override def optional: Dictionary.Reader[F, A, Option[B]] = Reader.Optional(this)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Dictionary.Reader[G, ?, B]

  object Reader:
    final case class Optional[F[+_], A, B](self: Dictionary.Reader[F, A, B]) extends Dictionary.Reader[F, A, Option[B]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Dictionary.Reader[G, ?, Option[B]] =
        copy(self = self.translate(fK))

    final case class Root[F[+_], A, +B <: F[Schema.Reader[F, ?, C]], C](key: F[Primitive.Required.Reader[A]], value: B)
        extends Dictionary.Reader[F, A, List[(A, C)]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Dictionary.Reader[G, ?, List[(A, C)]] =
        copy(key = fK(key), value = fK(value).map(_.translate(fK)))

    final case class Transform[F[+_], A, B, C](self: Dictionary.Reader[F, A, B], f: B => C)
        extends Dictionary.Reader[F, A, C]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Dictionary.Reader[G, ?, C] =
        copy(self = self.translate(fK))

  sealed trait Writer[+F[+_], +A, -B] extends Schema.Writer[F, A, B]:
    override def contramap[C](f: C => B): Dictionary.Writer[F, A, C] = Writer.Transform(this, f)
    override def optional: Dictionary.Writer[F, A, Option[B]] = Writer.Optional(this)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Dictionary.Writer[G, ?, B]

  object Writer:
    final case class Optional[F[+_], A, B](self: Dictionary.Writer[F, A, B]) extends Dictionary.Writer[F, A, Option[B]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Dictionary.Writer[G, ?, Option[B]] =
        copy(self = self.translate(fK))

    final case class Root[F[+_], A, +B <: F[Schema.Writer[F, ?, C]], C](key: F[Primitive.Required.Writer[A]], value: B)
        extends Dictionary.Writer[F, A, List[(A, C)]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Dictionary.Writer[G, ?, List[(A, C)]] =
        copy(key = fK(key), value = fK(value).map(_.translate(fK)))

    final case class Transform[F[+_], A, B, C](self: Dictionary.Writer[F, A, B], f: C => B)
        extends Dictionary.Writer[F, A, C]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Dictionary.Writer[G, ?, C] =
        copy(self = self.translate(fK))

  final case class Optional[F[+_], A, B](self: Dictionary[F, A, B]) extends Dictionary[F, A, Option[B]]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Dictionary[G, ?, Option[B]] =
      copy(self = self.translate(fK))

  final case class Root[F[+_], A, +B <: F[Schema[F, ?, C]], C](key: F[Primitive.Required[A]], value: B)
      extends Dictionary[F, A, List[(A, C)]]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Dictionary[G, ?, List[(A, C)]] =
      copy(key = fK(key), value = fK(value).map(_.translate(fK)))

  final case class Transform[F[+_], A, B, C](self: Dictionary[F, A, B], f: B => C, g: C => B)
      extends Dictionary[F, A, C]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Dictionary[G, ?, C] =
      copy(self = self.translate(fK))

// TODO
sealed trait Dynamic[+F[+_], +A, B] extends Schema[F, A, B]

object Dynamic:
  sealed trait Reader[+F[+_], +A, +B] extends Schema.Reader[F, A, B]

  sealed trait Writer[+F[+_], +A, -B] extends Schema.Writer[F, A, B]

sealed trait Enumeration[+F[+_], +A, B]
    extends Value[F, A, B],
      Enumeration.Reader[F, A, B],
      Enumeration.Writer[F, A, B]:
  override def imap[C](f: B => C)(g: C => B): Enumeration[F, A, C] = Enumeration.Transform(this, f, g)
  override def optional: Enumeration[F, A, Option[B]] = Enumeration.Optional(this)
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration[G, ?, B]

object Enumeration:
  sealed trait Required[+F[+_], +A, B]
      extends Value.Required[F, A, B],
        Enumeration[F, A, B],
        Enumeration.Required.Reader[F, A, B],
        Enumeration.Required.Writer[F, A, B]:
    override def imap[C](f: B => C)(g: C => B): Enumeration.Required[F, A, C] = Required.Transform(this, f, g)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Required[G, ?, B]

  object Required:
    sealed trait Reader[+F[+_], +A, +B] extends Value.Required.Reader[F, A, B], Enumeration.Reader[F, A, B]:
      override def map[C](f: B => C): Enumeration.Required.Reader[F, A, C] = Reader.Transform(this, f)
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Required.Reader[G, ?, B]

    object Reader:
      final case class Root[F[+_], +A <: F[Value.Required.Reader[F, ?, B]], B, C](
          schema: A,
          mapping: Mapping[C, B],
          writer: Schema.Writer[Identity, ?, B]
      ) extends Enumeration.Required.Reader[F, A, C]:
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Required.Reader[G, ?, C] =
          copy(schema = fK(schema).map(_.translate(fK)))

      final case class Transform[F[+_], A, B, C](self: Enumeration.Required.Reader[F, A, B], f: B => C)
          extends Enumeration.Required.Reader[F, A, C]:
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Required.Reader[G, ?, C] =
          copy(self = self.translate(fK))

    sealed trait Writer[+F[+_], +A, -B] extends Value.Required.Writer[F, A, B], Enumeration.Writer[F, A, B]:
      override def contramap[C](f: C => B): Enumeration.Required.Writer[F, A, C] = Writer.Transform(this, f)
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Required.Writer[G, ?, B]

    object Writer:
      final case class Root[F[+_], +A <: F[Value.Required.Writer[F, ?, B]], B, C](schema: A, f: C => B)
          extends Enumeration.Required.Writer[F, A, C]:
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Required.Writer[G, ?, C] =
          copy(schema = fK(schema).map(_.translate(fK)))

      final case class Transform[F[+_], A, B, C](self: Enumeration.Required.Writer[F, A, B], f: C => B)
          extends Enumeration.Required.Writer[F, A, C]:
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Required.Writer[G, ?, C] =
          copy(self = self.translate(fK))

    final case class Transform[F[+_], A, B, C](self: Enumeration.Required[F, A, B], f: B => C, g: C => B)
        extends Enumeration.Required[F, A, C]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Required[G, ?, C] =
        copy(self = self.translate(fK))

  sealed trait Reader[+F[+_], +A, +B] extends Value.Reader[F, A, B]:
    override def map[C](f: B => C): Enumeration.Reader[F, A, C] = Reader.Transform(this, f)
    override def optional: Enumeration.Reader[F, A, Option[B]] = Reader.Optional(this)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Reader[G, ?, B]

  object Reader:
    final case class Optional[F[+_], A, B](self: Enumeration.Reader[F, A, B])
        extends Enumeration.Reader[F, A, Option[B]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Reader[G, ?, Option[B]] =
        copy(self = self.translate(fK))

    final case class Root[F[+_], +A <: F[Value.Reader[F, ?, B]], B, C](
        schema: A,
        mapping: Mapping[C, B],
        writer: Schema.Writer[Identity, ?, B]
    ) extends Enumeration.Reader[F, A, C]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Reader[G, ?, C] =
        copy(schema = fK(schema).map(_.translate(fK)))

    final case class Transform[F[+_], A, B, C](self: Enumeration.Reader[F, A, B], f: B => C)
        extends Enumeration.Reader[F, A, C]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Reader[G, ?, C] =
        copy(self = self.translate(fK))

  sealed trait Writer[+F[+_], +A, -B] extends Value.Writer[F, A, B]:
    override def contramap[C](f: C => B): Enumeration.Writer[F, A, C] = Writer.Transform(this, f)
    override def optional: Enumeration.Writer[F, A, Option[B]] = Writer.Optional(this)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Writer[G, ?, B]

  object Writer:
    final case class Optional[F[+_], A, B](self: Enumeration.Writer[F, A, B])
        extends Enumeration.Writer[F, A, Option[B]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Writer[G, ?, Option[B]] =
        copy(self = self.translate(fK))

    final case class Transform[F[+_], A, B, C](self: Enumeration.Writer[F, A, B], f: C => B)
        extends Enumeration.Writer[F, A, C]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Writer[G, ?, C] =
        copy(self = self.translate(fK))

  final case class Optional[F[+_], A, B](self: Enumeration[F, A, B]) extends Enumeration[F, A, Option[B]]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration[G, ?, Option[B]] =
      copy(self = self.translate(fK))

  final case class Root[F[+_], +A <: F[Value[F, ?, B]], B, C](schema: A, mapping: Mapping[C, B])
      extends Enumeration[F, A, C]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration[G, ?, C] =
      copy(schema = fK(schema).map(_.translate(fK)))

  final case class Transform[F[+_], A, B, C](self: Enumeration[F, A, B], f: B => C, g: C => B)
      extends Enumeration[F, A, C]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration[G, ?, C] =
      copy(self = self.translate(fK))

sealed trait Primitive[A] extends Value[Nothing, Nothing, A], Primitive.Reader[A], Primitive.Writer[A]:
  override def imap[C](f: A => C)(g: C => A): Primitive[C] = ivalidate(Validation.lift(f))(g)
  final override def optional: Primitive[Option[A]] = Primitive.Optional(this)
  def ivalidate[B, C, D](validation: SchemaValidation.Primitive[A, B, C, D])(
      f: D => A
  ): Primitive[D] = Primitive.Transform(this, validation, f)
  override def translate[G[+_]: Functor](fK: [A] => Nothing => G[A]): Primitive[A] = this

object Primitive:
  sealed trait Required[A]
      extends Value.Required[Nothing, Nothing, A],
        Primitive[A],
        Primitive.Required.Reader[A],
        Primitive.Required.Writer[A]:
    final override def imap[C](f: A => C)(g: C => A): Primitive.Required[C] = ivalidate(Validation.lift(f))(g)
    override def ivalidate[B, C, D](validation: SchemaValidation.Primitive[A, B, C, D])(
        f: D => A
    ): Primitive.Required[D] = Required.Transform(this, validation, f)
    final override def translate[G[+_]: Functor](fK: [A] => Nothing => G[A]): Primitive.Required[A] = this

  object Required:
    sealed trait Reader[+A] extends Value.Required.Reader[Nothing, Nothing, A], Primitive.Reader[A]:
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

    sealed trait Writer[-A] extends Value.Required.Writer[Nothing, Nothing, A], Primitive.Writer[A]:
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

  sealed trait Reader[+A] extends Value.Reader[Nothing, Nothing, A]:
    def constraints: Chain[Constraint.Primitive[?]]
    override def map[C](f: A => C): Primitive.Reader[C] = validate(Validation.lift(f))
    override def optional: Primitive.Reader[Option[A]] = Reader.Optional(this)
    def validate[A1 >: A, B, C, D](
        validation: SchemaValidation.Primitive[A1, B, C, D]
    ): Primitive.Reader[D] = Reader.Transform(this, validation)
    def tpe: Type[?]
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

  sealed trait Writer[-A] extends Value.Writer[Nothing, Nothing, A]:
    override def contramap[B](f: B => A): Primitive.Writer[B] = Writer.Transform(this, f)
    override def optional: Primitive.Writer[Option[A]] = Writer.Optional(this)
    def tpe: Type[?]
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

sealed trait Product[+F[+_], +A, B] extends Schema[F, A, B], Product.Reader[F, A, B], Product.Writer[F, A, B]:
  override def imap[C](f: B => C)(g: C => B): Product[F, A, C] = Product.Transform(this, f, g)
  override def optional: Product[F, A, Option[B]] = Product.Optional(this)
  def product[G[+a] >: F[a], C, D](product: Product[G, C, D]): Product[G, A & C, (B, D)] =
    Product.Combine(this, product)
  def schemas: Chain[F[Schema[F, ?, ?]]]
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product[G, ?, B]

object Product:
  sealed trait Reader[+F[+_], +A, +B] extends Schema.Reader[F, A, B]:
    override def map[C](f: B => C): Product.Reader[F, A, C] = Reader.Transform(this, f)
    override def optional: Product.Reader[F, A, Option[B]] = Reader.Optional(this)
    def product[G[+a] >: F[a], C, D](product: Product.Reader[G, C, D]): Product.Reader[G, A & C, (B, D)] =
      Reader.Combine(this, product)
    def schemas: Chain[F[Schema.Reader[F, ?, ?]]]
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Reader[G, ?, B]

  object Reader:
    final case class Combine[F[+_], A, B, C, D](left: Product.Reader[F, A, B], right: Product.Reader[F, C, D])
        extends Product.Reader[F, A & C, (B, D)]:
      override def schemas: Chain[F[Schema.Reader[F, ?, ?]]] = left.schemas ++ right.schemas
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Reader[G, ?, (B, D)] =
        copy(left = left.translate(fK), right = right.translate(fK))

    final case class One[F[+_], +A <: F[Schema.Reader[F, ?, B]], B](schema: A) extends Product.Reader[F, A, B]:
      override def schemas: Chain[F[Schema.Reader[F, ?, ?]]] = Chain.one(schema)
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Reader[G, ?, B] =
        copy(schema = fK(schema).map(_.translate(fK)))

    final case class Optional[F[+_], A, B](self: Product.Reader[F, A, B]) extends Product.Reader[F, A, Option[B]]:
      export self.schemas
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Reader[G, ?, Option[B]] =
        copy(self = self.translate(fK))

    final case class Transform[F[+_], A, B, C](self: Product.Reader[F, A, B], f: B => C)
        extends Product.Reader[F, A, C]:
      export self.schemas
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Reader[G, ?, C] =
        copy(self = self.translate(fK))

  sealed trait Writer[+F[+_], +A, -B] extends Schema.Writer[F, A, B]:
    override def contramap[C](f: C => B): Product.Writer[F, A, C] =
      Writer.Transform(this, f)
    override def optional: Product.Writer[F, A, Option[B]] = Writer.Optional(this)
    def product[G[+a] >: F[a], C, D](product: Product.Writer[G, C, D]): Product.Writer[G, A & C, (B, D)] =
      Writer.Combine(this, product)
    def schemas: Chain[F[Schema.Writer[F, ?, ?]]]
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Writer[G, ?, B]

  object Writer:
    final case class Combine[F[+_], A, B, C, D](left: Product.Writer[F, A, B], right: Product.Writer[F, C, D])
        extends Product.Writer[F, A & C, (B, D)]:
      override def schemas: Chain[F[Schema.Writer[F, ?, ?]]] = left.schemas ++ right.schemas
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Writer[G, ?, (B, D)] =
        copy(left = left.translate(fK), right = right.translate(fK))

    final case class One[F[+_], +A <: F[Schema.Writer[F, ?, B]], B](schema: A) extends Product.Writer[F, A, B]:
      override def schemas: Chain[F[Schema.Writer[F, ?, ?]]] = Chain.one(schema)
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Writer[G, ?, B] =
        copy(schema = fK(schema).map(_.translate(fK)))

    final case class Optional[F[+_], A, B](self: Product.Writer[F, A, B]) extends Product.Writer[F, A, Option[B]]:
      export self.schemas
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Writer[G, ?, Option[B]] =
        copy(self = self.translate(fK))

    final case class Transform[F[+_], A, B, C](self: Product.Writer[F, A, B], f: C => B)
        extends Product.Writer[F, A, C]:
      export self.schemas
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Writer[G, ?, C] =
        copy(self = self.translate(fK))

  final case class Combine[F[+_], A, B, C, D](left: Product[F, A, B], right: Product[F, C, D])
      extends Product[F, A & C, (B, D)]:
    override def schemas: Chain[F[Schema[F, ?, ?]]] = left.schemas ++ right.schemas
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product[G, ?, (B, D)] =
      copy(left = left.translate(fK), right = right.translate(fK))

  case object Empty extends Product[Nothing, Nothing, Unit]:
    override def schemas: Chain[Nothing] = Chain.empty
    override def translate[G[+_]: Functor](fK: [A] => Nothing => G[A]): Product[G, ?, Unit] = this

  final case class One[F[+_], +A <: F[Schema[F, ?, B]], B](schema: A) extends Product[F, A, B]:
    override def schemas: Chain[F[Schema[F, ?, ?]]] = Chain.one(schema)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product[G, ?, B] =
      copy(schema = fK(schema).map(_.translate(fK)))

  final case class Optional[F[+_], A, B](self: Product[F, A, B]) extends Product[F, A, Option[B]]:
    export self.schemas
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product[G, ?, Option[B]] =
      copy(self = self.translate(fK))

  final case class Transform[F[+_], A, B, C](self: Product[F, A, B], f: B => C, g: C => B) extends Product[F, A, C]:
    export self.schemas
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product[G, ?, C] =
      copy(self = self.translate(fK))

sealed trait Record[+F[+_], +A, B] extends Schema[F, A, B], Record.Reader[F, A, B], Record.Writer[F, A, B]:
  override def nulls: Record.Null
  final def nulls(value: Record.Null): Record.Writer[F, A, B] = Record.Nulls(this, value)

  override def imap[C](f: B => C)(g: C => B): Record[F, A, C] = Record.Transform(this, f, g)
  override def optional: Record[F, A, Option[B]] = Record.Optional(this)
  def product[G[+a] >: F[a], C, D](product: Record[G, C, D]): Record[G, A & C, (B, D)] =
    Record.Combine(this, product)
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record[G, ?, B]

object Record:
  sealed trait Reader[+F[+_], +A, +B] extends Schema.Reader[F, A, B]:
    override def map[C](f: B => C): Record.Reader[F, A, C] = Reader.Transform(this, f)
    override def optional: Record.Reader[F, A, Option[B]] = Reader.Optional(this)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record.Reader[G, ?, B]

  object Reader:
    final case class One[F[+_], A, B](field: Field.Reader[F, A, B]) extends Record.Reader[F, A, B]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record.Reader[G, ?, B] =
        copy(field = field.translate(fK))

    final case class Optional[F[+_], A, B](self: Record.Reader[F, A, B]) extends Record.Reader[F, A, Option[B]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record.Reader[G, ?, Option[B]] =
        copy(self = self.translate(fK))

    final case class Transform[F[+_], A, B, C](self: Record.Reader[F, A, B], f: B => C) extends Record.Reader[F, A, C]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record.Reader[G, ?, C] =
        copy(self = self.translate(fK))

  sealed trait Writer[+F[+_], +A, -B] extends Schema.Writer[F, A, B]:
    def nulls: Record.Null

    override def contramap[C](f: C => B): Record.Writer[F, A, C] = Writer.Transform(this, f)
    override def optional: Record.Writer[F, A, Option[B]] = Writer.Optional(this)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record.Writer[G, ?, B]

  object Writer:
    final case class Combine[F[+_], A, B, C, D](left: Record.Writer[F, A, B], right: Record.Writer[F, C, D])
        extends Record.Writer[F, A & C, (B, D)]:
      override def nulls: Record.Null = Record.Null.Default
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record.Writer[G, ?, (B, D)] =
        copy(left = left.translate(fK), right = right.translate(fK))

    final case class Nulls[F[+_], A, B](self: Record.Writer[F, A, B], nulls: Record.Null)
        extends Record.Writer[F, A, B]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record.Writer[G, ?, B] =
        copy(self = self.translate(fK))

    final case class One[F[+_], A, B](field: Field.Writer[F, A, B]) extends Record.Writer[F, A, B]:
      override def nulls: Record.Null = Record.Null.Default
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record.Writer[G, ?, B] =
        copy(field = field.translate(fK))

    final case class Optional[F[+_], A, B](self: Record.Writer[F, A, B]) extends Record.Writer[F, A, Option[B]]:
      export self.nulls
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record.Writer[G, ?, Option[B]] =
        copy(self = self.translate(fK))

    final case class Transform[F[+_], A, B, C](self: Record.Writer[F, A, B], f: C => B) extends Record.Writer[F, A, C]:
      export self.nulls
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record.Writer[G, ?, C] =
        copy(self = self.translate(fK))

  case object Empty extends Record[Nothing, Nothing, Unit]:
    override def nulls: Record.Null = Record.Null.Default
    override def translate[G[+_]: Functor](fK: [A] => Nothing => G[A]): Record[G, ?, Unit] = this

  final case class Combine[F[+_], A, B, C, D](left: Record[F, A, B], right: Record[F, C, D])
      extends Record[F, A & C, (B, D)]:
    override def nulls: Record.Null = Record.Null.Default
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record[G, ?, (B, D)] =
      copy(left = left.translate(fK), right = right.translate(fK))

  final case class Nulls[F[+_], A, B](self: Record[F, A, B], nulls: Record.Null) extends Record[F, A, B]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record[G, ?, B] =
      copy(self = self.translate(fK))

  final case class One[F[+_], +A, B](field: Field[F, A, B]) extends Record[F, A, B]:
    override def nulls: Record.Null = Record.Null.Default
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record[G, ?, B] =
      copy(field = field.translate(fK))

  final case class Optional[F[+_], A, B](self: Record[F, A, B]) extends Record[F, A, Option[B]]:
    export self.nulls
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record[G, ?, Option[B]] =
      copy(self = self.translate(fK))

  final case class Transform[F[+_], A, B, C](self: Record[F, A, B], f: B => C, g: C => B) extends Record[F, A, C]:
    export self.nulls
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record[G, ?, C] =
      copy(self = self.translate(fK))

  enum Null:
    case Show
    case Hide

  object Null:
    val Default: Null = Show
    given Eq[Null] = Eq.fromUniversalEquals

sealed trait Sum[+F[+_], +A, B] extends Schema[F, A, B], Sum.Reader[F, A, B], Sum.Writer[F, A, B]:
  final override def discriminator(value: Sum.Discriminator): Sum[F, A, B] = Sum.Discriminators(this, value)

  override def branches: NonEmptyChain[Branch[F, ?, ?]]
  override def imap[C](f: B => C)(g: C => B): Sum[F, A, C] = Sum.Transform(this, f, g)
  override def optional: Sum[F, A, Option[B]] = Sum.Optional(this)
  def orElse[G[+a] >: F[a], C, D](sum: Sum[G, C, D]): Sum[G, A | C, Either[B, D]] = Sum.Combine(this, sum)
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum[G, ?, B]

object Sum:
  sealed trait Reader[+F[+_], +A, +B] extends Schema.Reader[F, A, B]:
    def discriminator: Sum.Discriminator
    def discriminator(value: Sum.Discriminator): Sum.Reader[F, A, B] = Reader.Discriminators(this, value)

    def branches: NonEmptyChain[Branch.Reader[F, ?, ?]]
    final override def map[C](f: B => C): Sum.Reader[F, A, C] = Reader.Transform(this, f)
    override def optional: Sum.Reader[F, A, Option[B]] = Reader.Optional(this)
    def orElse[G[+a] >: F[a], C, D](sum: Sum.Reader[G, C, D]): Sum.Reader[G, A | C, Either[B, D]] =
      Reader.Combine(this, sum)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum.Reader[G, ?, B]

  object Reader:
    final case class Combine[F[+_], A, B, C, D](left: Sum.Reader[F, A, B], right: Sum.Reader[F, C, D])
        extends Sum.Reader[F, A | C, Either[B, D]]:
      override def branches: NonEmptyChain[Branch.Reader[F, ?, ?]] = left.branches ++ right.branches
      override def discriminator: Discriminator = Discriminator.Default
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum.Reader[G, ?, Either[B, D]] =
        copy(left = left.translate(fK), right = right.translate(fK))

    final case class Discriminators[F[+_], A, B](self: Sum.Reader[F, A, B], discriminator: Sum.Discriminator)
        extends Sum.Reader[F, A, B]:
      export self.branches
      override def discriminator(value: Sum.Discriminator): Sum.Reader[F, A, B] = copy(discriminator = value)
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum.Reader[G, ?, B] =
        copy(self = self.translate(fK))

    final case class Optional[F[+_], A, B](self: Sum.Reader[F, A, B]) extends Sum.Reader[F, A, Option[B]]:
      export self.{branches, discriminator}
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum.Reader[G, ?, Option[B]] =
        copy(self = self.translate(fK))

    final case class Root[F[+_], A, B](branch: Branch.Reader[F, A, B]) extends Sum.Reader[F, A, B]:
      override def branches: NonEmptyChain[Branch.Reader[F, A, B]] = NonEmptyChain.one(branch)
      override def discriminator: Discriminator = Discriminator.Default
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum.Reader[G, ?, B] =
        copy(branch = branch.translate(fK))

    final case class Transform[F[+_], A, B, C](self: Sum.Reader[F, A, B], f: B => C) extends Sum.Reader[F, A, C]:
      export self.{branches, discriminator}
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum.Reader[G, ?, C] =
        copy(self = self.translate(fK))

  sealed trait Writer[+F[+_], +A, -B] extends Schema.Writer[F, A, B]:
    def discriminator: Discriminator
    def discriminator(value: Discriminator): Sum.Writer[F, A, B] = Writer.Discriminators(this, value)

    def branches: NonEmptyChain[Branch.Writer[F, ?, ?]]
    final override def contramap[C](f: C => B): Sum.Writer[F, A, C] = Writer.Transform(this, f)
    override def optional: Sum.Writer[F, A, Option[B]] = Writer.Optional(this)
    def orElse[G[+a] >: F[a], C, D](sum: Sum.Writer[G, C, D]): Sum.Writer[G, A | C, Either[B, D]] =
      Writer.Combine(this, sum)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum.Writer[G, ?, B]

  object Writer:
    final case class Combine[F[+_], A, B, C, D](left: Sum.Writer[F, A, B], right: Sum.Writer[F, C, D])
        extends Sum.Writer[F, A | C, Either[B, D]]:
      override def branches: NonEmptyChain[Branch.Writer[F, ?, ?]] = left.branches ++ right.branches
      override def discriminator: Sum.Discriminator = Sum.Discriminator.Default
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum.Writer[G, ?, Either[B, D]] =
        copy(left = left.translate(fK), right = right.translate(fK))

    final case class Discriminators[F[+_], A, B](self: Sum.Writer[F, A, B], discriminator: Sum.Discriminator)
        extends Sum.Writer[F, A, B]:
      export self.branches
      override def discriminator(value: Sum.Discriminator): Sum.Writer[F, A, B] = copy(discriminator = value)
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum.Writer[G, ?, B] =
        copy(self = self.translate(fK))

    final case class Optional[F[+_], A, B](self: Sum.Writer[F, A, B]) extends Sum.Writer[F, A, Option[B]]:
      export self.{branches, discriminator}
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum.Writer[G, ?, Option[B]] =
        copy(self = self.translate(fK))

    final case class Root[F[+_], A, B](branch: Branch.Writer[F, A, B]) extends Sum.Writer[F, A, B]:
      override def branches: NonEmptyChain[Branch.Writer[F, A, B]] = NonEmptyChain.one(branch)
      override def discriminator: Sum.Discriminator = Sum.Discriminator.Default
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum.Writer[G, ?, B] =
        copy(branch = branch.translate(fK))

    final case class Transform[F[+_], A, B, C](self: Sum.Writer[F, A, B], f: C => B) extends Sum.Writer[F, A, C]:
      export self.{branches, discriminator}
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum.Writer[G, ?, C] =
        copy(self = self.translate(fK))

  final case class Combine[F[+_], A, B, C, D](left: Sum[F, A, B], right: Sum[F, C, D])
      extends Sum[F, A | C, Either[B, D]]:
    override def branches: NonEmptyChain[Branch[F, ?, ?]] = left.branches ++ right.branches
    override def discriminator: Sum.Discriminator = Sum.Discriminator.Default
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum[G, ?, Either[B, D]] =
      copy(left = left.translate(fK), right = right.translate(fK))

  final case class Discriminators[F[+_], A, B](self: Sum[F, A, B], discriminator: Sum.Discriminator)
      extends Sum[F, A, B]:
    export self.branches
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum[G, ?, B] = copy(self = self.translate(fK))

  final case class Optional[F[+_], A, B](self: Sum[F, A, B]) extends Sum[F, A, Option[B]]:
    export self.{branches, discriminator}
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum[G, ?, Option[B]] =
      copy(self = self.translate(fK))

  final case class Root[F[+_], A, B](branch: Branch[F, A, B]) extends Sum[F, A, B]:
    override def branches: NonEmptyChain[Branch[F, A, B]] = NonEmptyChain.one(branch)
    override def discriminator: Sum.Discriminator = Sum.Discriminator.Default
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum[G, ?, B] =
      copy(branch = branch.translate(fK))

  final case class Transform[F[+_], A, B, C](self: Sum[F, A, B], f: B => C, g: C => B) extends Sum[F, A, C]:
    export self.{branches, discriminator}
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum[G, ?, C] =
      copy(self = self.translate(fK))

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

sealed trait Union[+F[+_], +A, B] extends Schema[F, A, B], Union.Reader[F, A, B], Union.Writer[F, A, B]:
  override def imap[C](f: B => C)(g: C => B): Union[F, A, C] = Union.Transform(this, f, g)
  override def optional: Union[F, A, Option[B]] = Union.Optional(this)
  def orElse[G[+a] >: F[a], C, D](union: Union[G, C, D]): Union[G, A | C, Either[B, D]] = Union.Combine(this, union)
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union[G, ?, B]

object Union:
  sealed trait Value[+F[+_], +A, B]
      extends Base.Value[F, A, B],
        Union[F, A, B],
        Union.Value.Reader[F, A, B],
        Union.Value.Writer[F, A, B]:
    override def imap[C](f: B => C)(g: C => B): Union.Value[F, A, C] =
      Value.Transform(this, f, g)
    final override def optional: Union.Value[F, A, Option[B]] = Value.Optional(this)
    def orElse[G[+a] >: F[a], C, D](union: Union.Value[G, C, D]): Union.Value[G, A | C, Either[B, D]] =
      Value.Combine(this, union)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value[G, ?, B]

  object Value:
    sealed trait Required[+F[+_], +A, B]
        extends Base.Value.Required[F, A, B],
          Union.Value[F, A, B],
          Union.Value.Required.Reader[F, A, B],
          Union.Value.Required.Writer[F, A, B]:
      override def imap[C](f: B => C)(g: C => B): Union.Value.Required[F, A, C] = Required.Transform(this, f, g)
      def orElse[G[+a] >: F[a], C, D](
          union: Union.Value.Required[G, C, D]
      ): Union.Value.Required[G, A | C, Either[B, D]] =
        Required.Combine(this, union)
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Required[G, ?, B]

    object Required:
      sealed trait Reader[+F[+_], +A, +B] extends Base.Value.Required.Reader[F, A, B], Union.Value.Reader[F, A, B]:
        override def map[C](f: B => C): Union.Value.Required.Reader[F, A, C] = Reader.Transform(this, f)
        def orElse[G[+a] >: F[a], C, D](
            union: Union.Value.Required.Reader[G, C, D]
        ): Union.Value.Required.Reader[G, A | C, Either[B, D]] =
          Reader.Combine(this, union)
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Required.Reader[G, ?, B]

      object Reader:
        final case class Combine[F[+_], A, B, C, D](
            left: Union.Value.Required.Reader[F, A, B],
            right: Union.Value.Required.Reader[F, C, D]
        ) extends Union.Value.Required.Reader[F, A | C, Either[B, D]]:
          override def translate[G[+_]: Functor](
              fK: [A] => F[A] => G[A]
          ): Union.Value.Required.Reader[G, ?, Either[B, D]] =
            copy(left = left.translate(fK), right = right.translate(fK))

        final case class Root[F[+_], +A <: F[Base.Value.Required.Reader[F, ?, B]], B](schema: A)
            extends Union.Value.Required.Reader[F, A, B]:
          override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Required.Reader[G, ?, B] =
            copy(schema = fK(schema).map(_.translate(fK)))

        final case class Transform[F[+_], A, B, C](self: Union.Value.Required.Reader[F, A, B], f: B => C)
            extends Union.Value.Required.Reader[F, A, C]:
          override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Required.Reader[G, ?, C] =
            copy(self = self.translate(fK))

      sealed trait Writer[+F[+_], +A, -B] extends Base.Value.Required.Writer[F, A, B], Union.Value.Writer[F, A, B]:
        override def contramap[C](f: C => B): Union.Value.Required.Writer[F, A, C] = Writer.Transform(this, f)
        def orElse[G[+a] >: F[a], C, D](
            union: Union.Value.Required.Writer[G, C, D]
        ): Union.Value.Required.Writer[G, A | C, Either[B, D]] =
          Writer.Combine(this, union)
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Required.Writer[G, ?, B]

      object Writer:
        final case class Combine[F[+_], A, B, C, D](
            left: Union.Value.Required.Writer[F, A, B],
            right: Union.Value.Required.Writer[F, C, D]
        ) extends Union.Value.Required.Writer[F, A | C, Either[B, D]]:
          override def translate[G[+_]: Functor](
              fK: [A] => F[A] => G[A]
          ): Union.Value.Required.Writer[G, ?, Either[B, D]] =
            copy(left = left.translate(fK), right = right.translate(fK))

        final case class Root[F[+_], +A <: F[Base.Value.Required.Writer[F, ?, B]], B](schema: A)
            extends Union.Value.Required.Writer[F, A, B]:
          override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Required.Writer[G, ?, B] =
            copy(schema = fK(schema).map(_.translate(fK)))

        final case class Transform[F[+_], A, B, C](self: Union.Value.Required.Writer[F, A, B], f: C => B)
            extends Union.Value.Required.Writer[F, A, C]:
          override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Required.Writer[G, ?, C] =
            copy(self = self.translate(fK))

      final case class Combine[F[+_], A, B, C, D](
          left: Union.Value.Required[F, A, B],
          right: Union.Value.Required[F, C, D]
      ) extends Union.Value.Required[F, A | C, Either[B, D]]:
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Required[G, ?, Either[B, D]] =
          copy(left = left.translate(fK), right = right.translate(fK))

      final case class Transform[F[+_], A, B, C](self: Union.Value.Required[F, A, B], f: B => C, g: C => B)
          extends Union.Value.Required[F, A, C]:
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Required[G, ?, C] =
          copy(self = self.translate(fK))

    sealed trait Reader[+F[+_], +A, +B] extends Base.Value.Reader[F, A, B], Union.Reader[F, A, B]:
      override def map[C](f: B => C): Union.Value.Reader[F, A, C] = Reader.Transform(this, f)
      override def optional: Union.Value.Reader[F, A, Option[B]] = Reader.Optional(this)
      def orElse[G[+a] >: F[a], C, D](union: Union.Value.Reader[G, C, D]): Union.Value.Reader[G, A | C, Either[B, D]] =
        Reader.Combine(this, union)
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Reader[G, ?, B]

    object Reader:
      final case class Combine[F[+_], A, B, C, D](left: Union.Value.Reader[F, A, B], right: Union.Value.Reader[F, C, D])
          extends Union.Value.Reader[F, A | C, Either[B, D]]:
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Reader[G, ?, Either[B, D]] =
          copy(left = left.translate(fK), right = right.translate(fK))

      final case class Optional[F[+_], A, B, C](self: Union.Value.Reader[F, A, B])
          extends Union.Value.Reader[F, A, Option[B]]:
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Reader[G, ?, Option[B]] =
          copy(self = self.translate(fK))

      final case class Transform[F[+_], A, B, C](self: Union.Value.Reader[F, A, B], f: B => C)
          extends Union.Value.Reader[F, A, C]:
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Reader[G, ?, C] =
          copy(self = self.translate(fK))

    sealed trait Writer[+F[+_], +A, -B] extends Base.Value.Writer[F, A, B], Union.Writer[F, A, B]:
      override def contramap[C](f: C => B): Union.Value.Writer[F, A, C] = Writer.Transform(this, f)
      override def optional: Union.Value.Writer[F, A, Option[B]] = Writer.Optional(this)
      def orElse[G[+a] >: F[a], C, D](union: Union.Value.Writer[G, C, D]): Union.Value.Writer[G, A | C, Either[B, D]] =
        Writer.Combine(this, union)
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Writer[G, ?, B]

    object Writer:
      final case class Combine[F[+_], A, B, C, D](left: Union.Value.Writer[F, A, B], right: Union.Value.Writer[F, C, D])
          extends Union.Value.Writer[F, A | C, Either[B, D]]:
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Writer[G, ?, Either[B, D]] =
          copy(left = left.translate(fK), right = right.translate(fK))

      final case class Optional[F[+_], A, B, C](self: Union.Value.Writer[F, A, B])
          extends Union.Value.Writer[F, A, Option[B]]:
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Writer[G, ?, Option[B]] =
          copy(self = self.translate(fK))

      final case class Transform[F[+_], A, B, C](self: Union.Value.Writer[F, A, B], f: C => B)
          extends Union.Value.Writer[F, A, C]:
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Writer[G, ?, C] =
          copy(self = self.translate(fK))

    final case class Combine[F[+_], A, B, C, D](left: Union.Value[F, A, B], right: Union.Value[F, C, D])
        extends Union.Value[F, A | C, Either[B, D]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value[G, ?, Either[B, D]] =
        copy(left = left.translate(fK), right = right.translate(fK))

    final case class Optional[F[+_], A, B](self: Union.Value[F, A, B]) extends Union.Value[F, A, Option[B]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value[G, ?, Option[B]] =
        copy(self = self.translate(fK))

    final case class Transform[F[+_], A, B, C](self: Union.Value[F, A, B], f: B => C, g: C => B)
        extends Union.Value[F, A, C]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value[G, ?, C] =
        copy(self = self.translate(fK))

  sealed trait Reader[+F[+_], +A, +B] extends Schema.Reader[F, A, B]:
    override def map[C](f: B => C): Union.Reader[F, A, C] = Reader.Transform(this, f)
    override def optional: Union.Reader[F, A, Option[B]] = Reader.Optional(this)
    def orElse[G[+a] >: F[a], C, D](union: Union.Reader[G, C, D]): Union.Reader[G, A | C, Either[B, D]] =
      Reader.Combine(this, union)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Reader[G, ?, B]

  object Reader:
    final case class Combine[F[+_], A, B, C, D](left: Union.Reader[F, A, B], right: Union.Reader[F, C, D])
        extends Union.Reader[F, A | C, Either[B, D]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Reader[G, ?, Either[B, D]] =
        copy(left = left.translate(fK), right = right.translate(fK))

    final case class Optional[F[+_], A, B](self: Union.Reader[F, A, B]) extends Union.Reader[F, A, Option[B]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Reader[G, ?, Option[B]] =
        copy(self = self.translate(fK))

    final case class Root[F[+_], +A <: F[Schema.Reader[F, ?, B]], B](schema: A) extends Union.Reader[F, A, B]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Reader[G, ?, B] =
        copy(schema = fK(schema).map(_.translate(fK)))

    final case class Transform[F[+_], A, B, C](self: Union.Reader[F, A, B], f: B => C) extends Union.Reader[F, A, C]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Reader[G, ?, C] =
        copy(self = self.translate(fK))

  sealed trait Writer[+F[+_], +A, -B] extends Schema.Writer[F, A, B]:
    override def contramap[C](f: C => B): Union.Writer[F, A, C] = Writer.Transform(this, f)
    override def optional: Union.Writer[F, A, Option[B]] = Writer.Optional(this)
    def orElse[G[+a] >: F[a], C, D](union: Union.Writer[G, C, D]): Union.Writer[G, A | C, Either[B, D]] =
      Writer.Combine(this, union)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Writer[G, ?, B]

  object Writer:
    final case class Combine[F[+_], A, B, C, D](left: Union.Writer[F, A, B], right: Union.Writer[F, C, D])
        extends Union.Writer[F, A | C, Either[B, D]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Writer[G, ?, Either[B, D]] =
        copy(left = left.translate(fK), right = right.translate(fK))

    final case class Optional[F[+_], A, B](self: Union.Writer[F, A, B]) extends Union.Writer[F, A, Option[B]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Writer[G, ?, Option[B]] =
        copy(self = self.translate(fK))

    final case class Root[F[+_], +A <: F[Schema.Writer[F, ?, B]], B](schema: A) extends Union.Writer[F, A, B]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Writer[G, ?, B] =
        copy(schema = fK(schema).map(_.translate(fK)))

    final case class Transform[F[+_], A, B, C](self: Union.Writer[F, A, B], f: C => B) extends Union.Writer[F, A, C]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Writer[G, ?, C] =
        copy(self = self.translate(fK))

  final case class Combine[F[+_], A, B, C, D](left: Union[F, A, B], right: Union[F, C, D])
      extends Union[F, A | C, Either[B, D]]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union[G, ?, Either[B, D]] =
      copy(left = left.translate(fK), right = right.translate(fK))

  final case class Optional[F[+_], A, B](self: Union[F, A, B]) extends Union[F, A, Option[B]]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union[G, ?, Option[B]] =
      copy(self = self.translate(fK))

  final case class Root[F[+_], +A <: F[Schema[F, ?, B]], B](schema: A) extends Union[F, A, B]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union[G, ?, B] =
      copy(schema = fK(schema).map(_.translate(fK)))

  final case class Transform[+F[+_], A, B, C](self: Union[F, A, B], f: B => C, g: C => B) extends Union[F, A, C]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union[G, ?, C] =
      copy(self = self.translate(fK))
