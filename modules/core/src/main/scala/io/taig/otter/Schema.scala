package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Schema.Reader
import cats.data.Chain
import io.taig.otter.validation.Validation
import cats.Functor
import io.taig.otter as Base
import scala.Product as SProduct
import io.taig.otter

sealed trait Schema[+F[+_], +A, B] extends Schema.Reader[F, A, B], Schema.Writer[F, A, B]:
  def imap[C](f: B => C)(g: C => B): Schema[F, A, C]
  override def optional: Schema[F, A, Option[B]]
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Schema[G, ?, B]

object Schema:
  sealed trait Reader[+F[+_], +A, +B] extends SProduct, Serializable:
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

object Value:
  sealed trait Required[+F[+_], +A, B]
      extends Value[F, A, B],
        Value.Required.Reader[F, A, B],
        Value.Required.Writer[F, A, B]:
    override def imap[C](f: B => C)(g: C => B): Value.Required[F, A, C]

  object Required:
    sealed trait Reader[+F[+_], +A, +B] extends Value.Reader[F, A, B]:
      override def map[C](f: B => C): Value.Required.Reader[F, A, C]

    sealed trait Writer[+F[+_], +A, -B] extends Value.Writer[F, A, B]:
      override def contramap[C](f: C => B): Value.Required.Writer[F, A, C]

  sealed trait Reader[+F[+_], +A, +B] extends Schema.Reader[F, A, B]:
    override def map[C](f: B => C): Value.Reader[F, A, C]
    override def optional: Value.Reader[F, A, Option[B]]

  sealed trait Writer[+F[+_], +A, -B] extends Schema.Writer[F, A, B]:
    override def contramap[C](f: C => B): Value.Writer[F, A, C]
    override def optional: Value.Writer[F, A, Option[B]]

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

    final case class Root[F[+_], A, B <: F[Schema.Reader[F, ?, C]], C](key: F[Primitive.Required.Reader[A]], value: B)
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

    final case class Root[F[+_], A, B <: F[Schema.Writer[F, ?, C]], C](key: F[Primitive.Required.Writer[A]], value: B)
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

  final case class Root[F[+_], A, B <: F[Schema[F, ?, C]], C](key: F[Primitive.Required[A]], value: B)
      extends Dictionary[F, A, List[(A, C)]]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Dictionary[G, ?, List[(A, C)]] =
      copy(key = fK(key), value = fK(value).map(_.translate(fK)))

  final case class Transform[F[+_], A, B, C](self: Dictionary[F, A, B], f: B => C, g: C => B)
      extends Dictionary[F, A, C]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Dictionary[G, ?, C] =
      copy(self = self.translate(fK))

sealed trait Dynamic[+F[+_], +A, B] extends Schema[F, A, B]

sealed trait Enumeration[+F[+_], +A, B]
    extends Value[F, A, B],
      Enumeration.Reader[F, A, B],
      Enumeration.Writer[F, A, B]:
  override def imap[C](f: B => C)(g: C => B): Enumeration[F, A, C] = ???
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration[G, ?, B] = ???

object Enumeration:
  sealed trait Required[+F[+_], +A, B]
      extends Value.Required[F, A, B],
        Enumeration[F, A, B],
        Enumeration.Required.Reader[F, A, B],
        Enumeration.Required.Writer[F, A, B]:
    override def imap[C](f: B => C)(g: C => B): Enumeration.Required[F, A, C] = ???
    override def optional: Enumeration.Required[F, A, Option[B]] = ???
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Required[G, ?, B] = ???

  object Required:
    sealed trait Reader[+F[+_], +A, +B] extends Value.Required.Reader[F, A, B], Enumeration.Reader[F, A, B]:
      override def map[C](f: B => C): Enumeration.Required.Reader[F, A, C] = ???
      override def optional: Enumeration.Required.Reader[F, A, Option[B]] = ???
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Required.Reader[G, ?, B] = ???

    sealed trait Writer[+F[+_], +A, -B] extends Value.Required.Writer[F, A, B], Enumeration.Writer[F, A, B]:
      override def contramap[C](f: C => B): Enumeration.Required.Writer[F, A, C] = ???
      override def optional: Enumeration.Required.Writer[F, A, Option[B]] = ???
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Required.Writer[G, ?, B] = ???

    object Writer:
      final case class Root[F[+_], +A <: F[Value.Required.Writer[F, ?, B]], B](schema: A)
          extends Enumeration.Required.Writer[F, A, B]

  sealed trait Reader[+F[+_], +A, +B] extends Value.Reader[F, A, B]:
    override def map[C](f: B => C): Enumeration.Reader[F, A, C] = ???
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Reader[G, ?, B] = ???

  sealed trait Writer[+F[+_], +A, -B] extends Value.Writer[F, A, B]:
    override def contramap[C](f: C => B): Enumeration.Writer[F, A, C] = ???
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Enumeration.Writer[G, ?, B] = ???

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
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product[G, ?, B]

object Product:
  sealed trait Reader[+F[+_], +A, +B] extends Schema.Reader[F, A, B]:
    override def map[C](f: B => C): Product.Reader[F, A, C] = Reader.Transform(this, f)
    override def optional: Product.Reader[F, A, Option[B]] = Reader.Optional(this)
    def product[G[+a] >: F[a], C, D](product: Product.Reader[G, C, D]): Product.Reader[G, A & C, (B, D)] =
      Reader.Combine(this, product)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Reader[G, ?, B]

  object Reader:
    final case class Combine[F[+_], A, B, C, D](left: Product.Reader[F, A, B], right: Product.Reader[F, C, D])
        extends Product.Reader[F, A & C, (B, D)]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Reader[G, ?, (B, D)] =
        copy(left = left.translate(fK), right = right.translate(fK))

    final case class One[F[+_], A <: F[Schema.Reader[F, ?, B]], B](schema: A) extends Product.Reader[F, A, B]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Reader[G, ?, B] =
        copy(schema = fK(schema).map(_.translate(fK)))

    final case class Optional[F[+_], A, B](self: Product.Reader[F, A, B]) extends Product.Reader[F, A, Option[B]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Reader[G, ?, Option[B]] =
        copy(self = self.translate(fK))

    final case class Transform[F[+_], A, B, C](self: Product.Reader[F, A, B], f: B => C)
        extends Product.Reader[F, A, C]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Reader[G, ?, C] =
        copy(self = self.translate(fK))

  sealed trait Writer[+F[+_], +A, -B] extends Schema.Writer[F, A, B]:
    override def contramap[C](f: C => B): Product.Writer[F, A, C] =
      Writer.Transform(this, f)
    override def optional: Product.Writer[F, A, Option[B]] = Writer.Optional(this)
    def product[G[+a] >: F[a], C, D](product: Product.Writer[G, C, D]): Product.Writer[G, A & C, (B, D)] =
      Writer.Combine(this, product)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Writer[G, ?, B]

  object Writer:
    final case class Combine[F[+_], A, B, C, D](left: Product.Writer[F, A, B], right: Product.Writer[F, C, D])
        extends Product.Writer[F, A & C, (B, D)]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Writer[G, ?, (B, D)] =
        copy(left = left.translate(fK), right = right.translate(fK))

    final case class One[F[+_], A <: F[Schema.Writer[F, ?, B]], B](schema: A) extends Product.Writer[F, A, B]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Writer[G, ?, B] =
        copy(schema = fK(schema).map(_.translate(fK)))

    final case class Optional[F[+_], A, B](self: Product.Writer[F, A, B]) extends Product.Writer[F, A, Option[B]]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Writer[G, ?, Option[B]] =
        copy(self = self.translate(fK))

    final case class Transform[F[+_], A, B, C](self: Product.Writer[F, A, B], f: C => B)
        extends Product.Writer[F, A, C]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product.Writer[G, ?, C] =
        copy(self = self.translate(fK))

  final case class Combine[F[+_], A, B, C, D](left: Product[F, A, B], right: Product[F, C, D])
      extends Product[F, A & C, (B, D)]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product[G, ?, (B, D)] =
      copy(left = left.translate(fK), right = right.translate(fK))

  case object Empty extends Product[Nothing, Nothing, Unit]:
    override def translate[G[+_]: Functor](fK: [A] => Nothing => G[A]): Product[G, ?, Unit] = this

  final case class One[F[+_], A <: F[Schema[F, ?, B]], B](schema: A) extends Product[F, A, B]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product[G, ?, B] =
      copy(schema = fK(schema).map(_.translate(fK)))

  final case class Optional[F[+_], A, B](self: Product[F, A, B]) extends Product[F, A, Option[B]]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product[G, ?, Option[B]] =
      copy(self = self.translate(fK))

  final case class Transform[F[+_], A, B, C](self: Product[F, A, B], f: B => C, g: C => B) extends Product[F, A, C]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Product[G, ?, C] =
      copy(self = self.translate(fK))

sealed trait Record[+F[+_], +A, B] extends Schema[F, A, B]

sealed trait Sum[+F[+_], +A, B] extends Schema[F, A, B]

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
    override def imap[C](f: B => C)(g: C => B): Union.Value[F, A, C] = ???
    override def optional: Union.Value[F, A, Option[B]] = ???

  object Value:
    sealed trait Required[+F[+_], +A, B]
        extends Base.Value.Required[F, A, B],
          Union.Value[F, A, B],
          Union.Value.Required.Reader[F, A, B],
          Union.Value.Required.Writer[F, A, B]:
      override def imap[C](f: B => C)(g: C => B): Union.Value.Required[F, A, C] = ???
      override def optional: Union.Value.Required[F, A, Option[B]] = ???

    object Required:
      sealed trait Reader[+F[+_], +A, +B] extends Base.Value.Required.Reader[F, A, B], Union.Value.Reader[F, A, B]:
        override def map[C](f: B => C): Union.Value.Required.Reader[F, A, C] = ???

      sealed trait Writer[+F[+_], +A, -B] extends Base.Value.Required.Writer[F, A, B], Union.Value.Writer[F, A, B]:
        override def contramap[C](f: C => B): Union.Value.Required.Writer[F, A, C] = ???

      object Writer:
        final case class Root[+F[+_], A <: F[Base.Value.Required.Writer[F, ?, B]], B](schema: A)
            extends Union.Value.Required.Writer[F, A, B]:
          override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union.Value.Required.Writer[G, ?, B] = ???

    sealed trait Reader[+F[+_], +A, +B] extends Base.Value.Reader[F, A, B], Union.Reader[F, A, B]:
      override def map[C](f: B => C): Union.Value.Reader[F, A, C] = ???
      override def optional: Union.Value.Reader[F, A, Option[B]] = ???

    sealed trait Writer[+F[+_], +A, -B] extends Base.Value.Writer[F, A, B], Union.Writer[F, A, B]:
      override def contramap[C](f: C => B): Union.Value.Writer[F, A, C] = ???
      override def optional: Union.Value.Writer[F, A, Option[B]] = ???

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

    final case class Root[+F[+_], A <: F[Schema.Reader[F, ?, B]], B](schema: A) extends Union.Reader[F, A, B]:
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

  final case class Root[+F[+_], +A <: F[Schema[F, ?, B]], B](schema: A) extends Union[F, A, B]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union[G, ?, B] =
      copy(schema = fK(schema).map(_.translate(fK)))

  final case class Transform[+F[+_], A, B, C](self: Union[F, A, B], f: B => C, g: C => B) extends Union[F, A, C]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Union[G, ?, C] =
      copy(self = self.translate(fK))
