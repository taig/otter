package io.taig.otter

import cats.data.Chain
import cats.syntax.all.*
import io.taig.enumeration.ext.Mapping
import io.taig.otter.validation.{Constraint, Validation}

import scala.collection.immutable.VectorMap

sealed abstract class Schema[A]:
  self =>
  type Self[a] <: Schema[a] { type Self[a] = self.Self[a] }

  def description: Option[String]
  def description(f: Option[String] => Option[String]): Self[A]
  final def description(value: Option[String]): Self[A] = description(_ => value)
  final def description(value: String): Self[A] = description(Some(value))

  def example: Option[A]
  def example(f: Option[A] => Option[A]): Self[A]
  final def example(value: Option[A]): Self[A] = example(_ => value)
  final def example(value: A): Self[A] = example(Some(value))

  def constraints: Chain[Constraint]
  def isOptional: Boolean

  def optional: Self[Option[A]]

  def ivalidate[B](validation: Validation[A, B])(g: B => A): Self[B]
  final def validate(validation: Validation[A, Unit]): Self[A] = ivalidate(validation.tap)(identity)
  final def imap[B](f: A => B)(g: B => A): Self[B] = ivalidate(Validation.lift(f))(g)
  final def const(value: A): Self[Unit] = imap(_ => ())(_ => value)

  final def toProduct: Product[A] = Product.Root(none, none, this)

object Schema: // extends ToSchemaOps:
  abstract class Value[A] extends Schema[A]:
    self =>
    override type Self[a] <: Value[a] { type Self[a] = self.Self[a] }

sealed abstract class Collection[F[a] <: Schema[a], A] extends Schema.Value[A]:
  final override type Self[a] = Collection[F, a]

  final override def optional: Collection[F, Option[A]] = Collection.Optional(this)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Collection[F, B] =
    Collection.Validate(this, validation, g)

object Collection:
  final case class Root[F[a] <: Schema[a], A](description: Option[String], example: Option[Chain[A]], schema: F[A])
      extends Collection[F, Chain[A]]:
    override def isOptional: Boolean = false
    override def constraints: Chain[Constraint] = Chain.empty
    override def description(f: Option[String] => Option[String]): Self[Chain[A]] = copy(description = f(description))
    override def example(f: Option[Chain[A]] => Option[Chain[A]]): Self[Chain[A]] = copy(example = f(example))

  final case class Optional[F[a] <: Schema[a], A](self: Collection[F, A]) extends Collection[F, Option[A]]:
    override def isOptional: Boolean = true
    override def constraints: Chain[Constraint] = self.constraints
    override def description: Option[String] = self.description
    override def description(f: Option[String] => Option[String]): Collection[F, Option[A]] =
      copy(self = self.description(f))
    override def example: Option[Option[A]] = self.example.map(_.some)
    override def example(f: Option[Option[A]] => Option[Option[A]]): Collection[F, Option[A]] =
      copy(self = self.example(fa => f(fa.map(_.some)).flatten))

  final case class Validate[F[a] <: Schema[a], A, B](
      self: Collection[F, A],
      validation: Validation[A, B],
      g: B => A
  ) extends Collection[F, B]:
    override def isOptional: Boolean = self.isOptional
    override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
    override def description: Option[String] = self.description
    override def description(f: Option[String] => Option[String]): Collection[F, B] = copy(self = self.description(f))
    override def example: Option[B] = self.example.flatMap(validation(_).toOption)
    override def example(f: Option[B] => Option[B]): Collection[F, B] =
      copy(self = self.example(fa => f(fa.flatMap(validation(_).toOption)).map(g)))

  def apply[F[a] <: Schema[a], A](schema: F[A]): Collection[F, Chain[A]] = Root(None, None, schema)

sealed abstract class Coproduct[A] extends Schema[A]:
  final override type Self[a] = Coproduct[a]

  def discriminator: Discriminator
  def discriminator(f: Discriminator => Discriminator): Coproduct[A]

  final override def optional: Coproduct[Option[A]] = Coproduct.Optional(this)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Coproduct[B] = ???

  final def orElse[B](coproduct: Coproduct[B]): Coproduct[A + B] = ???

  final def :+[B, C](branch: Branch[B, C]): Coproduct[A + C] = ???
  final def +:[B, C](branch: Branch[B, C]): Coproduct[C + A] = ???

object Coproduct:
  final case class Root[A, B](
      branch: Branch[A, B],
      description: Option[String],
      discriminator: Discriminator,
      example: Option[B]
  ) extends Coproduct[B]:
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = branch.isOptional
    override def description(f: Option[String] => Option[String]): Coproduct[B] = copy(description = f(description))
    override def discriminator(f: Discriminator => Discriminator): Coproduct[B] = copy(discriminator = f(discriminator))
    override def example(f: Option[B] => Option[B]): Coproduct[B] = copy(example = f(example))

  final case class Optional[A](self: Coproduct[A]) extends Coproduct[Option[A]]:
    override def constraints: Chain[Constraint] = self.constraints
    override def isOptional: Boolean = true
    override def discriminator: Discriminator = self.discriminator
    override def discriminator(f: Discriminator => Discriminator): Coproduct[Option[A]] =
      copy(self = self.discriminator(f))
    override def description: Option[String] = self.description
    override def description(f: Option[String] => Option[String]): Coproduct[Option[A]] =
      copy(self = self.description(f))
    override def example: Option[Option[A]] = self.example.map(_.some)
    override def example(f: Option[Option[A]] => Option[Option[A]]): Coproduct[Option[A]] =
      copy(self = self.example(fa => f(fa.map(_.some)).flatten))

sealed abstract class Dictionary[A] extends Schema.Value[A]:
  final override type Self[a] = Dictionary[a]

  final override def optional: Dictionary[Option[A]] = Dictionary.Optional(this)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Dictionary[B] =
    Dictionary.Validate(this, validation, g)

object Dictionary:
  final case class Root[A, B](
      description: Option[String],
      example: Option[VectorMap[A, B]],
      key: Schema.Value[A],
      value: Schema[B]
  ) extends Dictionary[VectorMap[A, B]]:
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def description(f: Option[String] => Option[String]): Dictionary[VectorMap[A, B]] =
      copy(description = f(description))
    override def example(f: Option[VectorMap[A, B]] => Option[VectorMap[A, B]]): Dictionary[VectorMap[A, B]] =
      copy(example = f(example))

  final case class Optional[A](self: Dictionary[A]) extends Dictionary[Option[A]]:
    override def constraints: Chain[Constraint] = self.constraints
    override def isOptional: Boolean = true
    override def description: Option[String] = self.description
    override def description(f: Option[String] => Option[String]): Dictionary[Option[A]] =
      copy(self = self.description(f))
    override def example: Option[Option[A]] = self.example.map(_.some)
    override def example(f: Option[Option[A]] => Option[Option[A]]): Dictionary[Option[A]] =
      copy(self = self.example(fa => f(fa.map(_.some)).flatten))

  final case class Validate[A, B](self: Dictionary[A], validation: Validation[A, B], g: B => A) extends Dictionary[B]:
    override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
    override def isOptional: Boolean = self.isOptional
    override def description: Option[String] = self.description
    override def description(f: Option[String] => Option[String]): Dictionary[B] = copy(self = self.description(f))
    override def example: Option[B] = self.example.flatMap(validation(_).toOption)
    override def example(f: Option[B] => Option[B]): Dictionary[B] =
      copy(self = self.example(fa => f(fa.flatMap(validation(_).toOption)).map(g)))

abstract class Enumeration[A] extends Schema.Value[A]:
  final override type Self[a] = Enumeration[a]

  final override def optional: Enumeration[Option[A]] = Enumeration.Optional(this)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Enumeration[B] =
    Enumeration.Validate(this, validation, g)

object Enumeration:
  final case class Root[A, B](
      description: Option[String],
      example: Option[B],
      mapping: Mapping[B, A],
      schema: Schema.Value[A]
  ) extends Enumeration[B]:
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def description(f: Option[String] => Option[String]): Enumeration[B] = copy(description = f(description))
    override def example(f: Option[B] => Option[B]): Enumeration[B] = copy(example = f(example))

  final case class Optional[A](self: Enumeration[A]) extends Enumeration[Option[A]]:
    override def constraints: Chain[Constraint] = self.constraints
    override def isOptional: Boolean = true
    override def description: Option[String] = self.description
    override def description(f: Option[String] => Option[String]): Enumeration[Option[A]] =
      copy(self = self.description(f))
    override def example: Option[Option[A]] = self.example.map(_.some)
    override def example(f: Option[Option[A]] => Option[Option[A]]): Enumeration[Option[A]] =
      copy(self = self.example(fa => f(fa.map(_.some)).flatten))

  final case class Validate[A, B](self: Enumeration[A], validation: Validation[A, B], g: B => A) extends Enumeration[B]:
    override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
    override def isOptional: Boolean = self.isOptional
    override def description: Option[String] = self.description
    override def description(f: Option[String] => Option[String]): Enumeration[B] = copy(self = self.description(f))
    override def example: Option[B] = self.example.flatMap(validation(_).toOption)
    override def example(f: Option[B] => Option[B]): Enumeration[B] =
      copy(self = self.example(fa => f(fa.flatMap(validation(_).toOption)).map(g)))

sealed abstract class Primitive[A] extends Schema.Value[A]:
  final override type Self[a] = Primitive[a]

  def format: Option[String]
  def format(f: Option[String] => Option[String]): Primitive[A]
  final def format(value: Option[String]): Primitive[A] = format(_ => value)
  final def format(value: String): Primitive[A] = format(Some(value))

  final override def optional: Primitive[Option[A]] = Primitive.Optional(this)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Primitive[B] =
    Primitive.Validate(this, validation, g)

object Primitive:
  final case class Root[A](description: Option[String], example: Option[A], format: Option[String], tpe: Type[A])
      extends Primitive[A]:
    override def isOptional: Boolean = false
    override def constraints: Chain[Constraint] = Chain.empty
    override def description(f: Option[String] => Option[String]): Primitive[A] = copy(description = f(description))
    override def example(f: Option[A] => Option[A]): Primitive[A] = copy(example = f(example))
    override def format(f: Option[String] => Option[String]): Primitive[A] = copy(format = f(format))

  final case class Optional[A](self: Primitive[A]) extends Primitive[Option[A]]:
    override def constraints: Chain[Constraint] = self.constraints
    override def isOptional: Boolean = true
    override def example: Option[Option[A]] = self.example.map(_.some)
    override def format: Option[String] = self.format
    override def description: Option[String] = self.description
    override def description(f: Option[String] => Option[String]): Primitive[Option[A]] =
      copy(self = self.description(f))
    override def example(f: Option[Option[A]] => Option[Option[A]]): Primitive[Option[A]] =
      copy(self = self.example(fa => f(fa.map(_.some)).flatten))
    override def format(f: Option[String] => Option[String]): Primitive[Option[A]] = copy(self = self.format(f))

  final case class Validate[A, B](self: Primitive[A], validation: Validation[A, B], g: B => A) extends Primitive[B]:
    override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
    override def isOptional: Boolean = self.isOptional
    override def example: Option[B] = self.example.flatMap(validation(_).toOption)
    override def format: Option[String] = self.format
    override def description: Option[String] = self.description
    override def format(f: Option[String] => Option[String]): Primitive[B] = copy(self = self.format(f))
    override def description(f: Option[String] => Option[String]): Primitive[B] = copy(self = self.description(f))
    override def example(f: Option[B] => Option[B]): Primitive[B] =
      copy(self = self.example(fa => f(fa.flatMap(validation(_).toOption)).map(g)))

  def apply[A](tpe: Type[A]): Primitive[A] = Root(None, None, None, tpe)

sealed abstract class Product[A] extends Schema[A]:
  final override type Self[a] = Product[a]

  final override def optional: Product[Option[A]] = Product.Optional(this)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Product[B] =
    Product.Validate(this, validation, g)

  final def prepend[B](schema: Schema[B]): Product[(B, A)] =
    Product.Prepend(description = none, example = none, schema, this)

  final def to[B](using evidence: Evidence.Product.Aux[B, A]): Product[B] = imap(evidence.from)(evidence.to)

object Product:
  final case class Root[A](description: Option[String], example: Option[A], schema: Schema[A]) extends Product[A]:
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def description(f: Option[String] => Option[String]): Product[A] = copy(description = f(description))
    override def example(f: Option[A] => Option[A]): Product[A] = copy(example = f(example))

  final case class Optional[A](self: Product[A]) extends Product[Option[A]]:
    override def constraints: Chain[Constraint] = self.constraints
    override def isOptional: Boolean = true
    override def description: Option[String] = self.description
    override def description(f: Option[String] => Option[String]): Product[Option[A]] =
      copy(self = self.description(f))
    override def example: Option[Option[A]] = self.example.map(_.some)
    override def example(f: Option[Option[A]] => Option[Option[A]]): Product[Option[A]] =
      copy(self = self.example(fa => f(fa.map(_.some)).flatten))

  final case class Validate[A, B](self: Product[A], validation: Validation[A, B], g: B => A) extends Product[B]:
    override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
    override def isOptional: Boolean = self.isOptional
    override def description: Option[String] = self.description
    override def description(f: Option[String] => Option[String]): Product[B] = copy(self = self.description(f))
    override def example: Option[B] = self.example.flatMap(validation(_).toOption)
    override def example(f: Option[B] => Option[B]): Product[B] =
      copy(self = self.example(fa => f(fa.flatMap(validation(_).toOption)).map(g)))

  final case class Prepend[A, B](
      description: Option[String],
      example: Option[(A, B)],
      schema: Schema[A],
      self: Product[B]
  ) extends Product[(A, B)]:
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = schema.isOptional && self.isOptional
    override def description(f: Option[String] => Option[String]): Product[(A, B)] = copy(description = f(description))
    override def example(f: Option[(A, B)] => Option[(A, B)]): Product[(A, B)] = copy(example = f(example))

sealed abstract class Record[A] extends Schema[A]:
  final override type Self[a] = Record[a]

  def nulls: Null
  def nulls(f: Null => Null): Record[A]
  final def nulls(value: Null): Record[A] = nulls(_ => value)

  final override def optional: Record[Option[A]] = Record.Optional(this)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Record[B] = ???

  final def zip[B](record: Record[B]): Record[(A, B)] = Record.Zip(this, record, none, none, Null.Default)

  final def to[B](using evidence: Evidence.Product.Aux[B, A]): Record[B] = imap(evidence.from)(evidence.to)

object Record: // extends ToRecordOps:
  final case class Empty(description: Option[String], example: Option[Unit], nulls: Null) extends Record[Unit]:
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def description(f: Option[String] => Option[String]): Record[Unit] = copy(description = f(description))
    override def nulls(f: Null => Null): Record[Unit] = copy(nulls = f(nulls))
    override def example(f: Option[Unit] => Option[Unit]): Record[Unit] = copy(example = f(example))

  final case class Prepend[A, B, C](
      field: Field[A, B],
      self: Record[C],
      description: Option[String],
      example: Option[(B, C)],
      nulls: Null
  ) extends Record[(B, C)]:
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = field.isOptional && self.isOptional
    override def description(f: Option[String] => Option[String]): Record[(B, C)] = copy(description = f(description))
    override def example(f: Option[(B, C)] => Option[(B, C)]): Record[(B, C)] = copy(example = f(example))
    override def nulls(f: Null => Null): Record[(B, C)] = copy(nulls = f(nulls))

  final case class Zip[A, B](
      left: Record[A],
      right: Record[B],
      description: Option[String],
      example: Option[(A, B)],
      nulls: Null
  ) extends Record[(A, B)]:
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = left.isOptional && right.isOptional
    override def description(f: Option[String] => Option[String]): Record[(A, B)] = copy(description = f(description))
    override def example(f: Option[(A, B)] => Option[(A, B)]): Record[(A, B)] = copy(example = f(example))
    override def nulls(f: Null => Null): Record[(A, B)] = copy(nulls = f(nulls))

  final case class Optional[A](self: Record[A]) extends Record[Option[A]]:
    override def constraints: Chain[Constraint] = self.constraints
    override def isOptional: Boolean = true
    override def nulls: Null = self.nulls
    override def nulls(f: Null => Null): Record[Option[A]] = ???
    override def description: Option[String] = self.description
    override def description(f: Option[String] => Option[String]): Record[Option[A]] = ???
    override def example: Option[Option[A]] = self.example.map(_.some)
    override def example(f: Option[Option[A]] => Option[Option[A]]): Record[Option[A]] = ???

  def apply[A, B](field: Field[A, B]): Record[B] = ???
