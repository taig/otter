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
import scala.reflect.ClassTag

sealed trait Schema[+F[+_], -A, +B, C] extends Schema.Reader[F, A, B, C], Schema.Writer[F, A, B, C]:
  override def default: Option[C] = ???
  override def default[C1 >: C](value: C1): Schema[F, A, B, C1] = ???

  def imap[D](f: C => D)(g: D => C): Schema[F, A, B, D]
  override def optional: Schema[F, A, B, Option[C]]
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Schema[G, A, ?, C]

object Schema:
  sealed trait Reader[+F[+_], -A, +B, +C] extends SProduct, Serializable:
    def default: Option[C] = ???
    def default[B1 >: C](value: B1): Schema.Reader[F, A, B, B1] = ???

    def map[D](f: C => D): Schema.Reader[F, A, B, D]
    def optional: Schema.Reader[F, A, B, Option[C]]
    def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Schema.Reader[G, A, ?, C]

  sealed trait Writer[+F[+_], -A, +B, -C] extends SProduct, Serializable:
    def contramap[D](f: D => C): Schema.Writer[F, A, B, D]
    def optional: Schema.Writer[F, A, B, Option[C]]
    def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Schema.Writer[G, A, ?, C]

sealed trait Value[+F[+_], -A, +B, C] extends Schema[F, A, B, C], Value.Reader[F, A, B, C], Value.Writer[F, A, B, C]:
  override def imap[D](f: C => D)(g: D => C): Value[F, A, B, D]
  override def optional: Value[F, A, B, Option[C]]
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Value[G, A, ?, C]

object Value:
  sealed trait Required[+F[+_], -A, +B, C]
      extends Value[F, A, B, C],
        Value.Required.Reader[F, A, B, C],
        Value.Required.Writer[F, A, B, C]:
    override def imap[D](f: C => D)(g: D => C): Value.Required[F, A, B, D]
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Value.Required[G, A, ?, C]

  object Required:
    sealed trait Reader[+F[+_], -A, +B, +C] extends Value.Reader[F, A, B, C]:
      override def map[D](f: C => D): Value.Required.Reader[F, A, B, D]
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Value.Required.Reader[G, A, ?, C]

    sealed trait Writer[+F[+_], -A, +B, -C] extends Value.Writer[F, A, B, C]:
      override def contramap[D](f: D => C): Value.Required.Writer[F, A, B, D]
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Value.Required.Writer[G, A, ?, C]

  sealed trait Reader[+F[+_], -A, +B, +C] extends Schema.Reader[F, A, B, C]:
    override def map[D](f: C => D): Value.Reader[F, A, B, D]
    override def optional: Value.Reader[F, A, B, Option[C]]
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Value.Reader[G, A, ?, C]

  sealed trait Writer[+F[+_], -A, +B, -C] extends Schema.Writer[F, A, B, C]:
    override def contramap[D](f: D => C): Value.Writer[F, A, B, D]
    override def optional: Value.Writer[F, A, B, Option[C]]
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Value.Writer[G, A, ?, C]

sealed trait Collection[+F[+_], -A, +B, C]
    extends Schema[F, A, B, C],
      Collection.Reader[F, A, B, C],
      Collection.Writer[F, A, B, C]:
  final override def imap[D](f: C => D)(g: D => C): Collection[F, A, B, D] = ivalidate(Validation.lift(f))(g)
  final def ivalidate[D, E](validation: SchemaValidation.Collection[C, D, E])(f: E => C): Collection[F, A, B, E] =
    Collection.Transform(this, validation, f)
  final override def optional: Collection[F, A, B, Option[C]] = Collection.Optional(this)
  override def schema: F[Schema[F, A, ?, ?]]
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection[G, A, ?, C]

object Collection:
  sealed trait Reader[+F[+_], -A, +B, +C] extends Schema.Reader[F, A, B, C]:
    def constraints: Chain[Constraint.Collection]
    final override def map[D](f: C => D): Collection.Reader[F, A, B, D] = validate(Validation.lift(f))
    override def optional: Collection.Reader[F, A, B, Option[C]] = Reader.Optional(this)
    def schema: F[Schema.Reader[F, ?, ?, ?]]
    final def validate[C1 >: C, D, E](
        validation: SchemaValidation.Collection[C1, D, E]
    ): Collection.Reader[F, A, B, E] = Reader.Transform(this, validation)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection.Reader[G, A, ?, C]

  object Reader:
    final case class Transform[+F[+_], A, B, C, D, E](
        self: Collection.Reader[F, A, B, C],
        validation: SchemaValidation.Collection[C, D, E]
    ) extends Collection.Reader[F, A, B, E]:
      export self.schema
      override def constraints: Chain[Constraint.Collection] = self.constraints ++ validation.constraints
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection.Reader[G, A, ?, E] =
        copy(self = self.translate(fK))

    final case class Optional[+F[+_], A, B, C](self: Collection.Reader[F, A, B, C])
        extends Collection.Reader[F, A, B, Option[C]]:
      export self.{constraints, schema}
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection.Reader[G, A, ?, Option[C]] =
        copy(self = self.translate(fK))

    final case class Root[F[+_], A, +B <: F[Schema.Reader[F, A, ?, C]], C](schema: B)
        extends Collection.Reader[F, A, B, Vector[C]]:
      override def constraints: Chain[Constraint.Collection] = Chain.empty
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection.Reader[G, A, ?, Vector[C]] =
        copy(schema = fK(schema).map(_.translate(fK)))

  sealed trait Writer[+F[+_], -A, +B, -C] extends Schema.Writer[F, A, B, C]:
    final def contramap[D](f: D => C): Collection.Writer[F, A, B, D] = Writer.Transform(this, f)
    def optional: Collection.Writer[F, A, B, Option[C]] = Writer.Optional(this)
    def schema: F[Schema.Writer[F, A, ?, ?]]
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection.Writer[G, A, ?, C]

  object Writer:
    final case class Transform[+F[+_], A, B, C, D](
        self: Collection.Writer[F, A, B, C],
        f: D => C
    ) extends Collection.Writer[F, A, B, D]:
      export self.schema
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection.Writer[G, A, ?, D] =
        copy(self = self.translate(fK))

    final case class Optional[+F[+_], A, B, C](self: Collection.Writer[F, A, B, C])
        extends Collection.Writer[F, A, B, Option[C]]:
      export self.schema
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection.Writer[G, A, ?, Option[C]] =
        copy(self = self.translate(fK))

    final case class Root[F[+_], A, +B <: F[Schema.Writer[F, A, ?, C]], C](schema: B)
        extends Collection.Writer[F, A, B, Vector[C]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection.Writer[G, A, ?, Vector[C]] =
        copy(schema = fK(schema).map(_.translate(fK)))

  final case class Optional[+F[+_], A, B, C](self: Collection[F, A, B, C]) extends Collection[F, A, B, Option[C]]:
    export self.{constraints, schema}
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection[G, A, ?, Option[C]] =
      copy(self = self.translate(fK))

  final case class Root[F[+_], A, +B <: F[Schema[F, A, ?, C]], C](schema: B) extends Collection[F, A, B, Vector[C]]:
    override def constraints: Chain[Constraint.Collection] = Chain.empty
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection[G, A, ?, Vector[C]] =
      copy(schema = fK(schema).map(_.translate(fK)))

  final case class Transform[+F[+_], A, B, C, D, E](
      self: Collection[F, A, B, C],
      validation: SchemaValidation.Collection[C, D, E],
      f: E => C
  ) extends Collection[F, A, B, E]:
    export self.schema
    override def constraints: Chain[Constraint.Collection] = self.constraints ++ validation.constraints
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Collection[G, A, ?, E] =
      copy(self = self.translate(fK))

sealed trait Dictionary[+F[+_], -A, +B, C]
    extends Schema[F, A, B, C],
      Dictionary.Reader[F, A, B, C],
      Dictionary.Writer[F, A, B, C]:
  override def imap[D](f: C => D)(g: D => C): Dictionary[F, A, B, D] = Dictionary.Transform(this, f, g)
  override def optional: Dictionary[F, A, B, Option[C]] = Dictionary.Optional(this)
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Dictionary[G, A, ?, C]

object Dictionary:
  sealed trait Reader[+F[+_], -A, +B, +C] extends Schema.Reader[F, A, B, C]:
    override def map[D](f: C => D): Dictionary.Reader[F, A, B, D] = Reader.Transform(this, f)
    override def optional: Dictionary.Reader[F, A, B, Option[C]] = Reader.Optional(this)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Dictionary.Reader[G, A, ?, C]

  object Reader:
    final case class Optional[F[+_], A, B, C](self: Dictionary.Reader[F, A, B, C])
        extends Dictionary.Reader[F, A, B, Option[C]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Dictionary.Reader[G, A, ?, Option[C]] =
        copy(self = self.translate(fK))

    final case class Root[F[+_], A, B, +C <: F[Schema.Reader[F, A, ?, D]], D](
        key: F[Primitive.Required.Reader[B]],
        value: C
    ) extends Dictionary.Reader[F, A, C, List[(B, D)]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Dictionary.Reader[G, A, ?, List[(B, D)]] =
        copy(key = fK(key), value = fK(value).map(_.translate(fK)))

    final case class Transform[F[+_], A, B, C, D](self: Dictionary.Reader[F, A, B, C], f: D => C)
        extends Dictionary.Reader[F, A, B, D]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Dictionary.Reader[G, A, ?, D] =
        copy(self = self.translate(fK))

  sealed trait Writer[+F[+_], -A, +B, -C] extends Schema.Writer[F, A, B, C]:
    override def contramap[D](f: D => C): Dictionary.Writer[F, A, B, D] = Writer.Transform(this, f)
    override def optional: Dictionary.Writer[F, A, B, Option[C]] = Writer.Optional(this)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Dictionary.Writer[G, A, ?, C]

  object Writer:
    final case class Optional[F[+_], A, B, C](self: Dictionary.Writer[F, A, B, C])
        extends Dictionary.Writer[F, A, B, Option[C]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Dictionary.Writer[G, A, ?, Option[C]] =
        copy(self = self.translate(fK))

    final case class Root[F[+_], A, B, +C <: F[Schema.Writer[F, A, ?, D]], D](
        key: F[Primitive.Required.Writer[B]],
        value: C
    ) extends Dictionary.Writer[F, A, C, List[(B, D)]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Dictionary.Writer[G, A, ?, List[(B, D)]] =
        copy(key = fK(key), value = fK(value).map(_.translate(fK)))

    final case class Transform[F[+_], A, B, C, D](self: Dictionary.Writer[F, A, B, C], f: D => C)
        extends Dictionary.Writer[F, A, B, D]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Dictionary.Writer[G, A, ?, D] =
        copy(self = self.translate(fK))

  final case class Optional[F[+_], A, B, C](self: Dictionary[F, A, B, C]) extends Dictionary[F, A, B, Option[C]]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Dictionary[G, A, ?, Option[C]] =
      copy(self = self.translate(fK))

  final case class Root[F[+_], A, B, +C <: F[Schema[F, A, ?, D]], D](key: F[Primitive.Required[B]], value: C)
      extends Dictionary[F, A, C, List[(B, D)]]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Dictionary[G, A, ?, List[(B, D)]] =
      copy(key = fK(key), value = fK(value).map(_.translate(fK)))

  final case class Transform[F[+_], A, B, C, D](self: Dictionary[F, A, B, C], f: C => D, g: D => C)
      extends Dictionary[F, A, B, D]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Dictionary[G, A, ?, D] =
      copy(self = self.translate(fK))

sealed trait Dynamic[A, B] extends Schema[Nothing, A, Nothing, B], Dynamic.Reader[A, B], Dynamic.Writer[A, B]:
  override def optional: Dynamic[A, Option[B]] = ???
  override def imap[C](f: B => C)(g: C => B): Dynamic[A, C] = ???
  final override def translate[G[+_]: Functor](fK: [A] => Nothing => G[A]): Dynamic[A, B] = this

object Dynamic:
  sealed trait Reader[A, +B] extends Schema.Reader[Nothing, A, Nothing, B]:
    override def optional: Dynamic.Reader[A, Option[B]] = ???
    override def map[C](f: B => C): Dynamic.Reader[A, C] = ???
    override def translate[G[+_]: Functor](fK: [A] => Nothing => G[A]): Dynamic.Reader[A, B] = this

  object Reader:
    final case class Root[A]() extends Dynamic.Reader[A, A]

  sealed trait Writer[A, -B] extends Schema.Writer[Nothing, A, Nothing, B]:
    override def optional: Dynamic.Writer[A, Option[B]] = ???
    override def contramap[C](f: C => B): Dynamic.Writer[A, C] = ???
    override def translate[G[+_]: Functor](fK: [A] => Nothing => G[A]): Dynamic.Writer[A, B] = this

  object Writer:
    final case class Root[A]() extends Dynamic.Writer[A, A]

  final case class Root[A]() extends Dynamic[A, A]

sealed trait Enumeration[+F[+_], -A, +B, C]
    extends Value[F, A, B, C],
      Enumeration.Reader[F, A, B, C],
      Enumeration.Writer[F, A, B, C]:
  override def imap[D](f: C => D)(g: D => C): Enumeration[F, A, B, D] = Enumeration.Transform(this, f, g)
  override def optional: Enumeration[F, A, B, Option[C]] = Enumeration.Optional(this)
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration[G, A, ?, C]

object Enumeration:
  sealed trait Required[+F[+_], -A, +B, C]
      extends Value.Required[F, A, B, C],
        Enumeration[F, A, B, C],
        Enumeration.Required.Reader[F, A, B, C],
        Enumeration.Required.Writer[F, A, B, C]:
    override def imap[D](f: C => D)(g: D => C): Enumeration.Required[F, A, B, D] = Required.Transform(this, f, g)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Required[G, A, ?, C]

  object Required:
    sealed trait Reader[+F[+_], -A, +B, +C] extends Value.Required.Reader[F, A, B, C], Enumeration.Reader[F, A, B, C]:
      override def map[D](f: C => D): Enumeration.Required.Reader[F, A, B, D] = Reader.Transform(this, f)
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Required.Reader[G, A, ?, C]

    object Reader:
      final case class Root[F[+_], A, +B <: F[Value.Required.Reader[F, A, ?, C]], C, D](
          schema: B,
          mapping: Mapping[D, C],
          writer: Schema.Writer[Identity, A, ?, C]
      ) extends Enumeration.Required.Reader[F, A, B, D]:
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Required.Reader[G, A, ?, D] =
          copy(schema = fK(schema).map(_.translate(fK)))

      final case class Transform[F[+_], A, B, C, D](self: Enumeration.Required.Reader[F, A, B, C], f: C => D)
          extends Enumeration.Required.Reader[F, A, B, D]:
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Required.Reader[G, A, ?, D] =
          copy(self = self.translate(fK))

    sealed trait Writer[+F[+_], -A, +B, -C] extends Value.Required.Writer[F, A, B, C], Enumeration.Writer[F, A, B, C]:
      override def contramap[D](f: D => C): Enumeration.Required.Writer[F, A, B, D] = Writer.Transform(this, f)
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Required.Writer[G, A, ?, C]

    object Writer:
      final case class Root[F[+_], A, +B <: F[Value.Required.Writer[F, A, ?, C]], C, D](schema: B, f: D => C)
          extends Enumeration.Required.Writer[F, A, B, D]:
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Required.Writer[G, A, ?, D] =
          copy(schema = fK(schema).map(_.translate(fK)))

      final case class Transform[F[+_], A, B, C, D](self: Enumeration.Required.Writer[F, A, B, C], f: D => C)
          extends Enumeration.Required.Writer[F, A, B, D]:
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Required.Writer[G, A, ?, D] =
          copy(self = self.translate(fK))

    final case class Transform[F[+_], A, B, C, D](self: Enumeration.Required[F, A, B, C], f: C => D, g: D => C)
        extends Enumeration.Required[F, A, B, D]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Required[G, A, ?, D] =
        copy(self = self.translate(fK))

  sealed trait Reader[+F[+_], -A, +B, +C] extends Value.Reader[F, A, B, C]:
    override def map[D](f: C => D): Enumeration.Reader[F, A, B, D] = Reader.Transform(this, f)
    override def optional: Enumeration.Reader[F, A, B, Option[C]] = Reader.Optional(this)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Reader[G, A, ?, C]

  object Reader:
    final case class Optional[F[+_], A, B, C](self: Enumeration.Reader[F, A, B, C])
        extends Enumeration.Reader[F, A, B, Option[C]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Reader[G, A, ?, Option[C]] =
        copy(self = self.translate(fK))

    final case class Root[F[+_], A, +B <: F[Value.Reader[F, A, ?, C]], C, D](
        schema: B,
        mapping: Mapping[D, C],
        writer: Schema.Writer[Identity, A, ?, C]
    ) extends Enumeration.Reader[F, A, B, D]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Reader[G, A, ?, D] =
        copy(schema = fK(schema).map(_.translate(fK)))

    final case class Transform[F[+_], A, B, C, D](self: Enumeration.Reader[F, A, B, C], f: C => D)
        extends Enumeration.Reader[F, A, B, D]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Reader[G, A, ?, D] =
        copy(self = self.translate(fK))

  sealed trait Writer[+F[+_], -A, +B, -C] extends Value.Writer[F, A, B, C]:
    override def contramap[D](f: D => C): Enumeration.Writer[F, A, B, D] = Writer.Transform(this, f)
    override def optional: Enumeration.Writer[F, A, B, Option[C]] = Writer.Optional(this)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Writer[G, A, ?, C]

  object Writer:
    final case class Optional[F[+_], A, B, C](self: Enumeration.Writer[F, A, B, C])
        extends Enumeration.Writer[F, A, B, Option[C]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Writer[G, A, ?, Option[C]] =
        copy(self = self.translate(fK))

    final case class Transform[F[+_], A, B, C, D](self: Enumeration.Writer[F, A, B, C], f: D => C)
        extends Enumeration.Writer[F, A, B, D]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Writer[G, A, ?, D] =
        copy(self = self.translate(fK))

  final case class Optional[F[+_], A, B, C](self: Enumeration[F, A, B, C]) extends Enumeration[F, A, B, Option[C]]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration[G, A, ?, Option[C]] =
      copy(self = self.translate(fK))

  final case class Root[F[+_], A, +B <: F[Value[F, A, ?, C]], C, D](schema: B, mapping: Mapping[D, C])
      extends Enumeration[F, A, B, D]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration[G, A, ?, D] =
      copy(schema = fK(schema).map(_.translate(fK)))

  final case class Transform[F[+_], A, B, C, D](self: Enumeration[F, A, B, C], f: C => D, g: D => C)
      extends Enumeration[F, A, B, D]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration[G, A, ?, D] =
      copy(self = self.translate(fK))

sealed trait Primitive[A] extends Value[Nothing, Any, Nothing, A], Primitive.Reader[A], Primitive.Writer[A]:
  override def imap[B](f: A => B)(g: B => A): Primitive[B] = ivalidate(Validation.lift(f))(g)
  final override def optional: Primitive[Option[A]] = Primitive.Optional(this)
  def ivalidate[B, C, D](validation: SchemaValidation.Primitive[A, B, C, D])(
      f: D => A
  ): Primitive[D] = Primitive.Transform(this, validation, f)
  override def translate[G[+_]: Functor](fK: [A] => Nothing => G[A]): Primitive[A] = this

object Primitive:
  sealed trait Required[A]
      extends Value.Required[Nothing, Any, Nothing, A],
        Primitive[A],
        Primitive.Required.Reader[A],
        Primitive.Required.Writer[A]:
    final override def imap[C](f: A => C)(g: C => A): Primitive.Required[C] = ivalidate(Validation.lift(f))(g)
    override def ivalidate[B, C, D](validation: SchemaValidation.Primitive[A, B, C, D])(
        f: D => A
    ): Primitive.Required[D] = Required.Transform(this, validation, f)
    final override def translate[G[+_]: Functor](fK: [A] => Nothing => G[A]): Primitive.Required[A] = this

  object Required:
    sealed trait Reader[+A] extends Value.Required.Reader[Nothing, Any, Nothing, A], Primitive.Reader[A]:
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

    sealed trait Writer[-A] extends Value.Required.Writer[Nothing, Any, Nothing, A], Primitive.Writer[A]:
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

  sealed trait Reader[+A] extends Value.Reader[Nothing, Any, Nothing, A]:
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

  sealed trait Writer[-A] extends Value.Writer[Nothing, Any, Nothing, A]:
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

sealed trait Product[+F[+_], -A, +B, C]
    extends Schema[F, A, B, C],
      Product.Reader[F, A, B, C],
      Product.Writer[F, A, B, C]:
  override def imap[D](f: C => D)(g: D => C): Product[F, A, B, D] = Product.Transform(this, f, g)
  override def optional: Product[F, A, B, Option[C]] = Product.Optional(this)
  def product[G[+a] >: F[a], A1 <: A, D, E](product: Product[G, A1, D, E]): Product[G, A1, B & D, (C, E)] =
    Product.Combine(this, product)
  def schemas: Chain[F[Schema[F, A, ?, ?]]]
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product[G, A, ?, C]

object Product:
  sealed trait Reader[+F[+_], -A, +B, +C] extends Schema.Reader[F, A, B, C]:
    override def map[D](f: C => D): Product.Reader[F, A, B, D] = Reader.Transform(this, f)
    override def optional: Product.Reader[F, A, B, Option[C]] = Reader.Optional(this)
    def product[G[+a] >: F[a], A1 <: A, D, E](
        product: Product.Reader[G, A1, D, E]
    ): Product.Reader[G, A1, B & D, (C, E)] =
      Reader.Combine(this, product)
    def schemas: Chain[F[Schema.Reader[F, A, ?, ?]]]
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Reader[G, A, ?, C]

  object Reader:
    final case class Combine[F[+_], A, B, C, D, E](left: Product.Reader[F, A, B, C], right: Product.Reader[F, A, D, E])
        extends Product.Reader[F, A, B & D, (C, E)]:
      override def schemas: Chain[F[Schema.Reader[F, A, ?, ?]]] = left.schemas ++ right.schemas
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Reader[G, A, ?, (C, E)] =
        copy(left = left.translate(fK), right = right.translate(fK))

    final case class One[F[+_], A, +B <: F[Schema.Reader[F, A, ?, C]], C](schema: B) extends Product.Reader[F, A, B, C]:
      override def schemas: Chain[F[Schema.Reader[F, A, ?, ?]]] = Chain.one(schema)
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Reader[G, A, ?, C] =
        copy(schema = fK(schema).map(_.translate(fK)))

    final case class Optional[F[+_], A, B, C](self: Product.Reader[F, A, B, C])
        extends Product.Reader[F, A, B, Option[C]]:
      export self.schemas
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Reader[G, A, ?, Option[C]] =
        copy(self = self.translate(fK))

    final case class Transform[F[+_], A, B, C, D](self: Product.Reader[F, A, B, C], f: C => D)
        extends Product.Reader[F, A, B, D]:
      export self.schemas
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Reader[G, A, ?, D] =
        copy(self = self.translate(fK))

  sealed trait Writer[+F[+_], -A, +B, -C] extends Schema.Writer[F, A, B, C]:
    override def contramap[D](f: D => C): Product.Writer[F, A, B, D] = Writer.Transform(this, f)
    override def optional: Product.Writer[F, A, B, Option[C]] = Writer.Optional(this)
    def product[G[+a] >: F[a], A1 <: A, D, E](
        product: Product.Writer[G, A1, D, E]
    ): Product.Writer[G, A1, B & D, (C, E)] =
      Writer.Combine(this, product)
    def schemas: Chain[F[Schema.Writer[F, A, ?, ?]]]
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Writer[G, A, ?, C]

  object Writer:
    final case class Combine[F[+_], A, B, C, D, E](left: Product.Writer[F, A, B, C], right: Product.Writer[F, A, D, E])
        extends Product.Writer[F, A, B & D, (C, E)]:
      override def schemas: Chain[F[Schema.Writer[F, A, ?, ?]]] = left.schemas ++ right.schemas
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Writer[G, A, ?, (C, E)] =
        copy(left = left.translate(fK), right = right.translate(fK))

    final case class One[F[+_], A, +B <: F[Schema.Writer[F, A, ?, C]], C](schema: B) extends Product.Writer[F, A, B, C]:
      override def schemas: Chain[F[Schema.Writer[F, A, ?, ?]]] = Chain.one(schema)
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Writer[G, A, ?, C] =
        copy(schema = fK(schema).map(_.translate(fK)))

    final case class Optional[F[+_], A, B, C](self: Product.Writer[F, A, B, C])
        extends Product.Writer[F, A, B, Option[C]]:
      export self.schemas
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Writer[G, A, ?, Option[C]] =
        copy(self = self.translate(fK))

    final case class Transform[F[+_], A, B, C, D](self: Product.Writer[F, A, B, C], f: D => C)
        extends Product.Writer[F, A, B, D]:
      export self.schemas
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Writer[G, A, ?, D] =
        copy(self = self.translate(fK))

  final case class Combine[F[+_], A, B, C, D, E](left: Product[F, A, B, C], right: Product[F, A, D, E])
      extends Product[F, A, B & D, (C, E)]:
    override def schemas: Chain[F[Schema[F, A, ?, ?]]] = left.schemas ++ right.schemas
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product[G, A, ?, (C, E)] =
      copy(left = left.translate(fK), right = right.translate(fK))

  case object Empty extends Product[Nothing, Any, Nothing, Unit]:
    override def schemas: Chain[Nothing] = Chain.empty
    override def translate[G[+_]: Functor](fK: [A] => Nothing => G[A]): Product[G, Any, ?, Unit] = this

  final case class One[F[+_], A, +B <: F[Schema[F, A, ?, C]], C](schema: B) extends Product[F, A, B, C]:
    override def schemas: Chain[F[Schema[F, A, ?, ?]]] = Chain.one(schema)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product[G, A, ?, C] =
      copy(schema = fK(schema).map(_.translate(fK)))

  final case class Optional[F[+_], A, B, C](self: Product[F, A, B, C]) extends Product[F, A, B, Option[C]]:
    export self.schemas
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product[G, A, ?, Option[C]] =
      copy(self = self.translate(fK))

  final case class Transform[F[+_], A, B, C, D](self: Product[F, A, B, C], f: C => D, g: D => C)
      extends Product[F, A, B, D]:
    export self.schemas
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product[G, A, ?, D] =
      copy(self = self.translate(fK))

sealed trait Record[+F[+_], -A, +B, C] extends Schema[F, A, B, C], Record.Reader[F, A, B, C], Record.Writer[F, A, B, C]:
  override def nulls: Record.Null
  final def nulls(value: Record.Null): Record.Writer[F, A, B, C] = Record.Nulls(this, value)

  def fields: Chain[Field[F, A, ?, ?]]
  override def imap[D](f: C => D)(g: D => C): Record[F, A, B, D] = Record.Transform(this, f, g)
  override def optional: Record[F, A, B, Option[C]] = Record.Optional(this)
  def product[G[+a] >: F[a], A1 <: A, D, E](product: Record[G, A1, D, E]): Record[G, A1, B & D, (C, E)] =
    Record.Combine(this, product)
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record[G, A, ?, C]

object Record:
  sealed trait Reader[+F[+_], -A, +B, +C] extends Schema.Reader[F, A, B, C]:
    def fields: Chain[Field.Reader[F, A, ?, ?]]
    override def map[D](f: C => D): Record.Reader[F, A, B, D] = Reader.Transform(this, f)
    override def optional: Record.Reader[F, A, B, Option[C]] = Reader.Optional(this)
    def product[G[+a] >: F[a], A1 <: A, D, E](
        product: Record.Reader[G, A1, D, E]
    ): Record.Reader[G, A1, B & D, (C, E)] =
      Reader.Combine(this, product)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record.Reader[G, A, ?, C]

  object Reader:
    final case class Combine[F[+_], A, B, C, D, E](left: Record.Reader[F, A, B, C], right: Record.Reader[F, A, D, E])
        extends Record.Reader[F, A, B & D, (C, E)]:
      override def fields: Chain[Field.Reader[F, A, ?, ?]] = left.fields ++ right.fields
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record.Reader[G, A, ?, (C, E)] =
        copy(left = left.translate(fK), right = right.translate(fK))

    final case class One[F[+_], A, B, C](field: Field.Reader[F, A, B, C]) extends Record.Reader[F, A, B, C]:
      override def fields: Chain[Field.Reader[F, A, ?, ?]] = Chain.one(field)
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record.Reader[G, A, ?, C] =
        copy(field = field.translate(fK))

    final case class Optional[F[+_], A, B, C](self: Record.Reader[F, A, B, C])
        extends Record.Reader[F, A, B, Option[C]]:
      export self.fields
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record.Reader[G, A, ?, Option[C]] =
        copy(self = self.translate(fK))

    final case class Transform[F[+_], A, B, C, D](self: Record.Reader[F, A, B, C], f: C => D)
        extends Record.Reader[F, A, B, D]:
      export self.fields
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record.Reader[G, A, ?, D] =
        copy(self = self.translate(fK))

  sealed trait Writer[+F[+_], -A, +B, -C] extends Schema.Writer[F, A, B, C]:
    def nulls: Record.Null

    override def contramap[D](f: D => C): Record.Writer[F, A, B, D] = Writer.Transform(this, f)
    def fields: Chain[Field.Writer[F, A, ?, ?]]
    override def optional: Record.Writer[F, A, B, Option[C]] = Writer.Optional(this)
    def product[G[+a] >: F[a], A1 <: A, D, E](
        product: Record.Writer[G, A1, D, E]
    ): Record.Writer[G, A1, B & D, (C, E)] =
      Writer.Combine(this, product)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record.Writer[G, A, ?, C]

  object Writer:
    final case class Combine[F[+_], A, B, C, D, E](left: Record.Writer[F, A, B, C], right: Record.Writer[F, A, D, E])
        extends Record.Writer[F, A, B & D, (C, E)]:
      override def fields: Chain[Field.Writer[F, A, ?, ?]] = left.fields ++ right.fields
      override def nulls: Record.Null = Record.Null.Default
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record.Writer[G, A, ?, (C, E)] =
        copy(left = left.translate(fK), right = right.translate(fK))

    final case class Nulls[F[+_], A, B, C](self: Record.Writer[F, A, B, C], nulls: Record.Null)
        extends Record.Writer[F, A, B, C]:
      export self.fields
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record.Writer[G, A, ?, C] =
        copy(self = self.translate(fK))

    final case class One[F[+_], A, B, C](field: Field.Writer[F, A, B, C]) extends Record.Writer[F, A, B, C]:
      override def nulls: Record.Null = Record.Null.Default
      override def fields: Chain[Field.Writer[F, A, ?, ?]] = Chain.one(field)
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record.Writer[G, A, ?, C] =
        copy(field = field.translate(fK))

    final case class Optional[F[+_], A, B, C](self: Record.Writer[F, A, B, C])
        extends Record.Writer[F, A, B, Option[C]]:
      export self.{fields, nulls}
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record.Writer[G, A, ?, Option[C]] =
        copy(self = self.translate(fK))

    final case class Transform[F[+_], A, B, C, D](self: Record.Writer[F, A, B, C], f: D => C)
        extends Record.Writer[F, A, B, D]:
      export self.{fields, nulls}
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record.Writer[G, A, ?, D] =
        copy(self = self.translate(fK))

  case object Empty extends Record[Nothing, Any, Nothing, Unit]:
    override def nulls: Record.Null = Record.Null.Default
    override def fields: Chain[Nothing] = Chain.empty
    override def translate[G[+_]: Functor](fK: [A] => Nothing => G[A]): Record[G, Any, ?, Unit] = this

  final case class Combine[F[+_], A, B, C, D, E](left: Record[F, A, B, C], right: Record[F, A, D, E])
      extends Record[F, A, B & D, (C, E)]:
    override def nulls: Record.Null = Record.Null.Default
    override def fields: Chain[Field[F, A, ?, ?]] = left.fields ++ right.fields
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record[G, A, ?, (C, E)] =
      copy(left = left.translate(fK), right = right.translate(fK))

  final case class Nulls[F[+_], A, B, C](self: Record[F, A, B, C], nulls: Record.Null) extends Record[F, A, B, C]:
    export self.fields
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record[G, A, ?, C] =
      copy(self = self.translate(fK))

  final case class One[F[+_], A, B, C](field: Field[F, A, B, C]) extends Record[F, A, B, C]:
    override def nulls: Record.Null = Record.Null.Default
    override def fields: Chain[Field[F, A, ?, ?]] = Chain.one(field)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record[G, A, ?, C] =
      copy(field = field.translate(fK))

  final case class Optional[F[+_], A, B, C](self: Record[F, A, B, C]) extends Record[F, A, B, Option[C]]:
    export self.{fields, nulls}
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record[G, A, ?, Option[C]] =
      copy(self = self.translate(fK))

  final case class Transform[F[+_], A, B, C, D](self: Record[F, A, B, C], f: C => D, g: D => C)
      extends Record[F, A, B, D]:
    export self.{fields, nulls}
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Record[G, A, ?, D] =
      copy(self = self.translate(fK))

  enum Null:
    case Show
    case Hide

  object Null:
    val Default: Null = Show
    given Eq[Null] = Eq.fromUniversalEquals

sealed trait Sum[+F[+_], -A, +B, C] extends Schema[F, A, B, C], Sum.Reader[F, A, B, C], Sum.Writer[F, A, B, C]:
  final override def discriminator(value: Sum.Discriminator): Sum[F, A, B, C] = Sum.Discriminators(this, value)

  override def branches: NonEmptyChain[Branch[F, A, ?, ?]]
  override def imap[D](f: C => D)(g: D => C): Sum[F, A, B, D] = Sum.Transform(this, f, g)
  override def optional: Sum[F, A, B, Option[C]] = Sum.Optional(this)
  def orElse[G[+a] >: F[a], A1 <: A, D, E](sum: Sum[G, A1, D, E]): Sum[G, A1, B | D, Either[C, E]] =
    Sum.Combine(this, sum)
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum[G, A, ?, C]

object Sum:
  sealed trait Reader[+F[+_], -A, +B, +C] extends Schema.Reader[F, A, B, C]:
    def discriminator: Sum.Discriminator
    def discriminator(value: Sum.Discriminator): Sum.Reader[F, A, B, C] = Reader.Discriminators(this, value)

    def branches: NonEmptyChain[Branch.Reader[F, A, ?, ?]]
    final override def map[D](f: C => D): Sum.Reader[F, A, B, D] = Reader.Transform(this, f)
    override def optional: Sum.Reader[F, A, B, Option[C]] = Reader.Optional(this)
    def orElse[G[+a] >: F[a], A1 <: A, D, E](sum: Sum.Reader[G, A1, D, E]): Sum.Reader[G, A1, B | D, Either[C, E]] =
      Reader.Combine(this, sum)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum.Reader[G, A, ?, C]

  object Reader:
    final case class Combine[F[+_], A, B, C, D, E](left: Sum.Reader[F, A, B, C], right: Sum.Reader[F, A, D, E])
        extends Sum.Reader[F, A, B | D, Either[C, E]]:
      override def branches: NonEmptyChain[Branch.Reader[F, A, ?, ?]] = left.branches ++ right.branches
      override def discriminator: Discriminator = Discriminator.Default
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum.Reader[G, A, ?, Either[C, E]] =
        copy(left = left.translate(fK), right = right.translate(fK))

    final case class Discriminators[F[+_], A, B, C](self: Sum.Reader[F, A, B, C], discriminator: Sum.Discriminator)
        extends Sum.Reader[F, A, B, C]:
      export self.branches
      override def discriminator(value: Sum.Discriminator): Sum.Reader[F, A, B, C] = copy(discriminator = value)
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum.Reader[G, A, ?, C] =
        copy(self = self.translate(fK))

    final case class Optional[F[+_], A, B, C](self: Sum.Reader[F, A, B, C]) extends Sum.Reader[F, A, B, Option[C]]:
      export self.{branches, discriminator}
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum.Reader[G, A, ?, Option[C]] =
        copy(self = self.translate(fK))

    final case class Root[F[+_], A, B, C](branch: Branch.Reader[F, A, B, C]) extends Sum.Reader[F, A, B, C]:
      override def branches: NonEmptyChain[Branch.Reader[F, A, B, C]] = NonEmptyChain.one(branch)
      override def discriminator: Discriminator = Discriminator.Default
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum.Reader[G, A, ?, C] =
        copy(branch = branch.translate(fK))

    final case class Transform[F[+_], A, B, C, D](self: Sum.Reader[F, A, B, C], f: C => D)
        extends Sum.Reader[F, A, B, D]:
      export self.{branches, discriminator}
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum.Reader[G, A, ?, D] =
        copy(self = self.translate(fK))

  sealed trait Writer[+F[+_], -A, +B, -C] extends Schema.Writer[F, A, B, C]:
    def discriminator: Discriminator
    def discriminator(value: Discriminator): Sum.Writer[F, A, B, C] = Writer.Discriminators(this, value)

    def branches: NonEmptyChain[Branch.Writer[F, A, ?, ?]]
    final override def contramap[D](f: D => C): Sum.Writer[F, A, B, D] = Writer.Transform(this, f)
    override def optional: Sum.Writer[F, A, B, Option[C]] = Writer.Optional(this)
    def orElse[G[+a] >: F[a], A1 <: A, D, E](sum: Sum.Writer[G, A1, D, E]): Sum.Writer[G, A1, B | D, Either[C, E]] =
      Writer.Combine(this, sum)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum.Writer[G, A, ?, C]

  object Writer:
    final case class Combine[F[+_], A, B, C, D, E](left: Sum.Writer[F, A, B, C], right: Sum.Writer[F, A, D, E])
        extends Sum.Writer[F, A, B | D, Either[C, E]]:
      override def branches: NonEmptyChain[Branch.Writer[F, A, ?, ?]] = left.branches ++ right.branches
      override def discriminator: Sum.Discriminator = Sum.Discriminator.Default
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum.Writer[G, A, ?, Either[C, E]] =
        copy(left = left.translate(fK), right = right.translate(fK))

    final case class Discriminators[F[+_], A, B, C](self: Sum.Writer[F, A, B, C], discriminator: Sum.Discriminator)
        extends Sum.Writer[F, A, B, C]:
      export self.branches
      override def discriminator(value: Sum.Discriminator): Sum.Writer[F, A, B, C] = copy(discriminator = value)
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum.Writer[G, A, ?, C] =
        copy(self = self.translate(fK))

    final case class Optional[F[+_], A, B, C](self: Sum.Writer[F, A, B, C]) extends Sum.Writer[F, A, B, Option[C]]:
      export self.{branches, discriminator}
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum.Writer[G, A, ?, Option[C]] =
        copy(self = self.translate(fK))

    final case class Root[F[+_], A, B, C](branch: Branch.Writer[F, A, B, C]) extends Sum.Writer[F, A, B, C]:
      override def branches: NonEmptyChain[Branch.Writer[F, A, B, C]] = NonEmptyChain.one(branch)
      override def discriminator: Sum.Discriminator = Sum.Discriminator.Default
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum.Writer[G, A, ?, C] =
        copy(branch = branch.translate(fK))

    final case class Transform[F[+_], A, B, C, D](self: Sum.Writer[F, A, B, C], f: D => C)
        extends Sum.Writer[F, A, B, D]:
      export self.{branches, discriminator}
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum.Writer[G, A, ?, D] =
        copy(self = self.translate(fK))

  final case class Combine[F[+_], A, B, C, D, E](left: Sum[F, A, B, C], right: Sum[F, A, D, E])
      extends Sum[F, A, B | D, Either[C, E]]:
    override def branches: NonEmptyChain[Branch[F, A, ?, ?]] = left.branches ++ right.branches
    override def discriminator: Sum.Discriminator = Sum.Discriminator.Default
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum[G, A, ?, Either[C, E]] =
      copy(left = left.translate(fK), right = right.translate(fK))

  final case class Discriminators[F[+_], A, B, C](self: Sum[F, A, B, C], discriminator: Sum.Discriminator)
      extends Sum[F, A, B, C]:
    export self.branches
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum[G, A, ?, C] = copy(self = self.translate(fK))

  final case class Optional[F[+_], A, B, C](self: Sum[F, A, B, C]) extends Sum[F, A, B, Option[C]]:
    export self.{branches, discriminator}
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum[G, A, ?, Option[C]] =
      copy(self = self.translate(fK))

  final case class Root[F[+_], A, B, C](branch: Branch[F, A, B, C]) extends Sum[F, A, B, C]:
    override def branches: NonEmptyChain[Branch[F, A, B, C]] = NonEmptyChain.one(branch)
    override def discriminator: Sum.Discriminator = Sum.Discriminator.Default
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum[G, A, ?, C] =
      copy(branch = branch.translate(fK))

  final case class Transform[F[+_], A, B, C, D](self: Sum[F, A, B, C], f: C => D, g: D => C) extends Sum[F, A, B, D]:
    export self.{branches, discriminator}
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Sum[G, A, ?, D] =
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

sealed trait Union[+F[+_], -A, +B, C] extends Schema[F, A, B, C], Union.Reader[F, A, B, C], Union.Writer[F, A, B, C]:
  override def imap[D](f: C => D)(g: D => C): Union[F, A, B, D] = Union.Transform(this, f, g)
  override def optional: Union[F, A, B, Option[C]] = Union.Optional(this)
  def orElse[G[+a] >: F[a], A1 <: A, D, E](union: Union[G, A1, D, E]): Union[G, A1, B | D, Either[C, E]] =
    Union.Combine(this, union)
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union[G, A, ?, C]

object Union:
  sealed trait Value[+F[+_], -A, +B, C]
      extends Base.Value[F, A, B, C],
        Union[F, A, B, C],
        Union.Value.Reader[F, A, B, C],
        Union.Value.Writer[F, A, B, C]:
    override def imap[D](f: C => D)(g: D => C): Union.Value[F, A, B, D] = Value.Transform(this, f, g)
    final override def optional: Union.Value[F, A, B, Option[C]] = Value.Optional(this)
    def orElse[G[+a] >: F[a], A1 <: A, D, E](union: Union.Value[G, A1, D, E]): Union.Value[G, A1, B | D, Either[C, E]] =
      Value.Combine(this, union)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value[G, A, ?, C]

  object Value:
    sealed trait Required[+F[+_], -A, +B, C]
        extends Base.Value.Required[F, A, B, C],
          Union.Value[F, A, B, C],
          Union.Value.Required.Reader[F, A, B, C],
          Union.Value.Required.Writer[F, A, B, C]:
      override def imap[D](f: C => D)(g: D => C): Union.Value.Required[F, A, B, D] = Required.Transform(this, f, g)
      def orElse[G[+a] >: F[a], A1 <: A, D, E](
          union: Union.Value.Required[G, A1, D, E]
      ): Union.Value.Required[G, A1, B | D, Either[C, E]] = Required.Combine(this, union)
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Required[G, A, ?, C]

    object Required:
      sealed trait Reader[+F[+_], -A, +B, +C]
          extends Base.Value.Required.Reader[F, A, B, C],
            Union.Value.Reader[F, A, B, C]:
        override def map[D](f: C => D): Union.Value.Required.Reader[F, A, B, D] = Reader.Transform(this, f)
        def orElse[G[+a] >: F[a], A1 <: A, D, E](
            union: Union.Value.Required.Reader[G, A1, D, E]
        ): Union.Value.Required.Reader[G, A1, B | D, Either[C, E]] = Reader.Combine(this, union)
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Required.Reader[G, A, ?, C]

      object Reader:
        final case class Combine[F[+_], A, B, C, D, E](
            left: Union.Value.Required.Reader[F, A, B, C],
            right: Union.Value.Required.Reader[F, A, D, E]
        ) extends Union.Value.Required.Reader[F, A, B | D, Either[C, E]]:
          override def translate[G[+_]: Functor](
              fK: [A] => F[A] => G[A]
          ): Union.Value.Required.Reader[G, A, ?, Either[C, E]] =
            copy(left = left.translate(fK), right = right.translate(fK))

        final case class Root[F[+_], A, +B <: F[Base.Value.Required.Reader[F, A, ?, C]], C](schema: B)
            extends Union.Value.Required.Reader[F, A, B, C]:
          override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Required.Reader[G, A, ?, C] =
            copy(schema = fK(schema).map(_.translate(fK)))

        final case class Transform[F[+_], A, B, C, D](self: Union.Value.Required.Reader[F, A, B, C], f: C => D)
            extends Union.Value.Required.Reader[F, A, B, D]:
          override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Required.Reader[G, A, ?, D] =
            copy(self = self.translate(fK))

      sealed trait Writer[+F[+_], -A, +B, -C]
          extends Base.Value.Required.Writer[F, A, B, C],
            Union.Value.Writer[F, A, B, C]:
        override def contramap[D](f: D => C): Union.Value.Required.Writer[F, A, B, D] = Writer.Transform(this, f)
        def orElse[G[+a] >: F[a], A1 <: A, D, E](
            union: Union.Value.Required.Writer[G, A1, D, E]
        ): Union.Value.Required.Writer[G, A1, B | D, Either[C, E]] = Writer.Combine(this, union)
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Required.Writer[G, A, ?, C]

      object Writer:
        final case class Combine[F[+_], A, B, C, D, E](
            left: Union.Value.Required.Writer[F, A, B, C],
            right: Union.Value.Required.Writer[F, A, D, E]
        ) extends Union.Value.Required.Writer[F, A, B | D, Either[C, E]]:
          override def translate[G[+_]: Functor](
              fK: [A] => F[A] => G[A]
          ): Union.Value.Required.Writer[G, A, ?, Either[C, E]] =
            copy(left = left.translate(fK), right = right.translate(fK))

        final case class Root[F[+_], A, +B <: F[Base.Value.Required.Writer[F, A, ?, C]], C](schema: B)
            extends Union.Value.Required.Writer[F, A, B, C]:
          override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Required.Writer[G, A, ?, C] =
            copy(schema = fK(schema).map(_.translate(fK)))

        final case class Transform[F[+_], A, B, C, D](self: Union.Value.Required.Writer[F, A, B, C], f: D => C)
            extends Union.Value.Required.Writer[F, A, B, D]:
          override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Required.Writer[G, A, ?, D] =
            copy(self = self.translate(fK))

      final case class Combine[F[+_], A, B, C, D, E](
          left: Union.Value.Required[F, A, B, C],
          right: Union.Value.Required[F, A, D, E]
      ) extends Union.Value.Required[F, A, B | D, Either[C, E]]:
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Required[G, A, ?, Either[C, E]] =
          copy(left = left.translate(fK), right = right.translate(fK))

      final case class Transform[F[+_], A, B, C, D](self: Union.Value.Required[F, A, B, C], f: C => D, g: D => C)
          extends Union.Value.Required[F, A, B, D]:
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Required[G, A, ?, D] =
          copy(self = self.translate(fK))

    sealed trait Reader[+F[+_], -A, +B, +C] extends Base.Value.Reader[F, A, B, C], Union.Reader[F, A, B, C]:
      override def map[D](f: C => D): Union.Value.Reader[F, A, B, D] = Reader.Transform(this, f)
      override def optional: Union.Value.Reader[F, A, B, Option[C]] = Reader.Optional(this)
      def orElse[G[+a] >: F[a], A1 <: A, D, E](
          union: Union.Value.Reader[G, A1, D, E]
      ): Union.Value.Reader[G, A1, B | D, Either[C, E]] = Reader.Combine(this, union)
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Reader[G, A, ?, C]

    object Reader:
      final case class Combine[F[+_], A, B, C, D, E](
          left: Union.Value.Reader[F, A, B, C],
          right: Union.Value.Reader[F, A, D, E]
      ) extends Union.Value.Reader[F, A, B | D, Either[C, E]]:
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Reader[G, A, ?, Either[C, E]] =
          copy(left = left.translate(fK), right = right.translate(fK))

      final case class Optional[F[+_], A, B, C, D](self: Union.Value.Reader[F, A, B, C])
          extends Union.Value.Reader[F, A, B, Option[C]]:
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Reader[G, A, ?, Option[C]] =
          copy(self = self.translate(fK))

      final case class Transform[F[+_], A, B, C, D](self: Union.Value.Reader[F, A, B, C], f: C => D)
          extends Union.Value.Reader[F, A, B, D]:
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Reader[G, A, ?, D] =
          copy(self = self.translate(fK))

    sealed trait Writer[+F[+_], -A, +B, -C] extends Base.Value.Writer[F, A, B, C], Union.Writer[F, A, B, C]:
      override def contramap[D](f: D => C): Union.Value.Writer[F, A, B, D] = Writer.Transform(this, f)
      override def optional: Union.Value.Writer[F, A, B, Option[C]] = Writer.Optional(this)
      def orElse[G[+a] >: F[a], A1 <: A, D, E](
          union: Union.Value.Writer[G, A1, D, E]
      ): Union.Value.Writer[G, A1, B | D, Either[C, E]] = Writer.Combine(this, union)
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Writer[G, A, ?, C]

    object Writer:
      final case class Combine[F[+_], A, B, C, D, E](
          left: Union.Value.Writer[F, A, B, C],
          right: Union.Value.Writer[F, A, D, E]
      ) extends Union.Value.Writer[F, A, B | D, Either[C, E]]:
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Writer[G, A, ?, Either[C, E]] =
          copy(left = left.translate(fK), right = right.translate(fK))

      final case class Optional[F[+_], A, B, C, D](self: Union.Value.Writer[F, A, B, C])
          extends Union.Value.Writer[F, A, B, Option[C]]:
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Writer[G, A, ?, Option[C]] =
          copy(self = self.translate(fK))

      final case class Transform[F[+_], A, B, C, D](self: Union.Value.Writer[F, A, B, C], f: D => C)
          extends Union.Value.Writer[F, A, B, D]:
        override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Writer[G, A, ?, D] =
          copy(self = self.translate(fK))

    final case class Combine[F[+_], A, B, C, D, E](left: Union.Value[F, A, B, C], right: Union.Value[F, A, D, E])
        extends Union.Value[F, A, B | D, Either[C, E]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value[G, A, ?, Either[C, E]] =
        copy(left = left.translate(fK), right = right.translate(fK))

    final case class Optional[F[+_], A, B, C](self: Union.Value[F, A, B, C]) extends Union.Value[F, A, B, Option[C]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value[G, A, ?, Option[C]] =
        copy(self = self.translate(fK))

    final case class Transform[F[+_], A, B, C, D](self: Union.Value[F, A, B, C], f: C => D, g: D => C)
        extends Union.Value[F, A, B, D]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value[G, A, ?, D] =
        copy(self = self.translate(fK))

  sealed trait Reader[+F[+_], -A, +B, +C] extends Schema.Reader[F, A, B, C]:
    override def map[D](f: C => D): Union.Reader[F, A, B, D] = Reader.Transform(this, f)
    override def optional: Union.Reader[F, A, B, Option[C]] = Reader.Optional(this)
    def orElse[G[+a] >: F[a], A1 <: A, D, E](
        union: Union.Reader[G, A1, D, E]
    ): Union.Reader[G, A1, B | D, Either[C, E]] =
      Reader.Combine(this, union)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Reader[G, A, ?, C]

  object Reader:
    final case class Combine[F[+_], A, B, C, D, E](left: Union.Reader[F, A, B, C], right: Union.Reader[F, A, D, E])
        extends Union.Reader[F, A, B | D, Either[C, E]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Reader[G, A, ?, Either[C, E]] =
        copy(left = left.translate(fK), right = right.translate(fK))

    final case class Optional[F[+_], A, B, C](self: Union.Reader[F, A, B, C]) extends Union.Reader[F, A, B, Option[C]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Reader[G, A, ?, Option[C]] =
        copy(self = self.translate(fK))

    final case class Root[F[+_], A, +B <: F[Schema.Reader[F, A, ?, C]], C](schema: B) extends Union.Reader[F, A, B, C]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Reader[G, A, ?, C] =
        copy(schema = fK(schema).map(_.translate(fK)))

    final case class Transform[F[+_], A, B, C, D](self: Union.Reader[F, A, B, C], f: C => D)
        extends Union.Reader[F, A, B, D]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Reader[G, A, ?, D] =
        copy(self = self.translate(fK))

  sealed trait Writer[+F[+_], -A, +B, -C] extends Schema.Writer[F, A, B, C]:
    override def contramap[D](f: D => C): Union.Writer[F, A, B, D] = Writer.Transform(this, f)
    override def optional: Union.Writer[F, A, B, Option[C]] = Writer.Optional(this)
    def orElse[G[+a] >: F[a], A1 <: A, D, E](
        union: Union.Writer[G, A1, D, E]
    ): Union.Writer[G, A1, B | D, Either[C, E]] =
      Writer.Combine(this, union)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Writer[G, A, ?, C]

  object Writer:
    final case class Combine[F[+_], A, B, C, D, E](left: Union.Writer[F, A, B, C], right: Union.Writer[F, A, D, E])
        extends Union.Writer[F, A, B | D, Either[C, E]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Writer[G, A, ?, Either[C, E]] =
        copy(left = left.translate(fK), right = right.translate(fK))

    final case class Optional[F[+_], A, B, C](self: Union.Writer[F, A, B, C]) extends Union.Writer[F, A, B, Option[C]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Writer[G, A, ?, Option[C]] =
        copy(self = self.translate(fK))

    final case class Root[F[+_], A, +B <: F[Schema.Writer[F, A, ?, C]], C](schema: B) extends Union.Writer[F, A, B, C]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Writer[G, A, ?, C] =
        copy(schema = fK(schema).map(_.translate(fK)))

    final case class Transform[F[+_], A, B, C, D](self: Union.Writer[F, A, B, C], f: D => C)
        extends Union.Writer[F, A, B, D]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Writer[G, A, ?, D] =
        copy(self = self.translate(fK))

  final case class Combine[F[+_], A, B, C, D, E](left: Union[F, A, B, C], right: Union[F, A, D, E])
      extends Union[F, A, B | D, Either[C, E]]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union[G, A, ?, Either[C, E]] =
      copy(left = left.translate(fK), right = right.translate(fK))

  final case class Optional[F[+_], A, B, C](self: Union[F, A, B, C]) extends Union[F, A, B, Option[C]]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union[G, A, ?, Option[C]] =
      copy(self = self.translate(fK))

  final case class Root[F[+_], A, +B <: F[Schema[F, A, ?, C]], C](schema: B) extends Union[F, A, B, C]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union[G, A, ?, C] =
      copy(schema = fK(schema).map(_.translate(fK)))

  final case class Transform[+F[+_], A, B, C, D](self: Union[F, A, B, C], f: C => D, g: D => C)
      extends Union[F, A, B, D]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union[G, A, ?, D] =
      copy(self = self.translate(fK))
