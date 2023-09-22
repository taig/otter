package io.taig.otter.schema

import cats.syntax.all.*
import cats.data.Chain
import io.taig.otter.validation.{Constraint, Validation}
import scala.collection.immutable.VectorMap
import io.taig.enumeration.ext.Mapping

sealed abstract class Schema[A]:
  self =>
  type Self[a] <: Schema[a] { type Self[a] = self.Self[a] }

  trait Optional:
    this: Self[Option[A]] =>
    export self.constraints
    final override def isOptional: Boolean = true

  trait Validate[B](validation: Validation[A, B]):
    this: Self[B] =>
    export self.isOptional
    final override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints

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

  // final def toProduct: Product[A] = Product(this)

object Schema: // extends ToSchemaOps:
  abstract class Value[A] extends Schema[A]:
    self =>
    override type Self[a] <: Value[a] { type Self[a] = self.Self[a] }

sealed abstract class Collection[F[a] <: Schema[a], A] extends Schema[A]:
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

  final case class Validate[F[a] <: Schema[a], A, B](self: Collection[F, A], validation: Validation[A, B], g: B => A)
      extends Collection[F, B]:
    override def isOptional: Boolean = self.isOptional
    override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
    override def description: Option[String] = self.description
    override def description(f: Option[String] => Option[String]): Collection[F, B] = copy(self = self.description(f))
    override def example: Option[B] = self.example.flatMap(validation(_).toOption)
    override def example(f: Option[B] => Option[B]): Collection[F, B] =
      copy(self = self.example(fa => f(fa.flatMap(validation(_).toOption)).map(g)))

sealed abstract class Dictionary[A] extends Schema[A]:
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
    override def description(f: Option[String] => Option[String]): Primitive[B] =
      copy(self = self.description(f))
    override def example(f: Option[B] => Option[B]): Primitive[B] =
      copy(self = self.example(fa => f(fa.flatMap(validation(_).toOption)).map(g)))
