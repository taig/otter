package io.taig.otter

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.enumeration.ext.Mapping
import io.taig.otter.validation.{Constraint, Validation, Violation, Violations}

import scala.collection.immutable.VectorMap

abstract class Schema[A]:
  self =>
  type Self[a] <: Schema[a]

  def constraints: Chain[Constraint]
  def isOptional: Boolean

  def description: Option[String]
  def description(f: Option[String] => Option[String]): Self[A]
  final def description(value: Option[String]): Self[A] = description(_ => value)
  final def description(value: String): Self[A] = description(Some(value))

  def optional: Self[Option[A]]

  def ivalidate[B](validation: Validation[A, B])(g: B => A): Self[B]
  final def validate(validation: Validation[A, Unit]): Self[A] = ivalidate(validation.tap)(identity)
  final def imap[B](f: A => B)(g: B => A): Self[B] = ivalidate(Validation.lift(f))(g)

  final def orElse[B](schema: Schema[B]): Schema[Either[A, B]] = ???
  final def :+[B](schema: Schema[B]): Schema[Either[A, B]] = orElse(schema)
  final def +:[B](schema: Schema[B]): Schema[Either[B, A]] = schema.orElse(this)

  def encode(a: A): Data
  def decode(data: Data): Validated[Violations, A]

object Schema:
  extension [A <: Matchable](self: Schema[A])
    inline def |[B <: Matchable](schema: Schema[B]): Schema[A | B] = self
      .orElse(schema)
      .imap {
        case Left(a)  => a
        case Right(b) => b
      } {
        case a: A => Left(a)
        case b: B => Right(b)
      }

//sealed abstract class Schema[+A]:
//  self =>
//  type Self[+a] <: Schema[a]
//  type Of <: Schema[?]
//
//  def description: Option[String]
//  def description(f: Option[String] => Option[String]): Self[A]
//  final def description(value: Option[String]): Self[A] = description(_ => value)
//  final def description(value: String): Self[A] = description(Some(value))
//
//  def constraints: Chain[Constraint]
//  def isOptional: Boolean
//
//  def optional: Self[Option[A]]
//
//  def ivalidate[B >: A, C](validation: Validation[B, C])(g: C => B): Self[C]
//  final def validate(validation: Validation[A, Unit]): Self[A] = ivalidate(validation.tap)(identity)
//  final def imap[B >: A, C](f: B => C)(g: C => B): Self[C] = ivalidate(Validation.lift(f))(g)
//  // final def const(value: A): Self[Unit] = imap(_ => ())(_ => value)
//
//  final def toProduct: Schema.Product[A] = Schema.Product(this)
//
//object Schema: // extends ToSchemaOps:
//  type Of[+A <: Schema[?], +B] = Schema[B] { type Of <: A }
//
//  sealed abstract class Value[+A] extends Schema[A]:
//    override type Self[+a] <: Schema.Value[a]
//    override type Of <: Schema.Value[?]
//
//  sealed abstract class Collection[F[a] <: Schema[a], +A] extends Schema[A]:
//    final override type Self[a] = Collection[F, a]
//    final override type Of <: Schema.Collection[F, ?]
//
//    final override def optional: Collection[F, Option[A]] = Collection.Optional(this)
//
//    final override def ivalidate[B >: A, C](validation: Validation[B, C])(g: C => B): Collection[F, C] =
//      Collection.Validate(this, validation, g)
//
//  object Collection:
//    final case class Root[F[a] <: Schema[a], A](schema: F[A], description: Option[String])
//        extends Collection[F, Chain[A]]:
//      override def isOptional: Boolean = false
//      override def constraints: Chain[Constraint] = Chain.empty
//      override def description(f: Option[String] => Option[String]): Self[Chain[A]] = copy(description = f(description))
//
//    final case class Optional[F[a] <: Schema[a], A](self: Collection[F, A]) extends Collection[F, Option[A]]:
//      override def isOptional: Boolean = true
//      override def constraints: Chain[Constraint] = self.constraints
//      override def description: Option[String] = self.description
//      override def description(f: Option[String] => Option[String]): Collection[F, Option[A]] =
//        copy(self = self.description(f))
//
//    final case class Validate[F[a] <: Schema[a], A, B](
//        self: Collection[F, A],
//        validation: Validation[A, B],
//        g: B => A
//    ) extends Collection[F, B]:
//      override def isOptional: Boolean = self.isOptional
//      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
//      override def description: Option[String] = self.description
//      override def description(f: Option[String] => Option[String]): Collection[F, B] = copy(self = self.description(f))
//
//    def apply[F[a] <: Schema[a], A](schema: F[A]): Collection[F, Chain[A]] = Root(schema, None)
//
//  sealed abstract class Coproduct[+A] extends Schema[A]:
//    final override type Self[+a] = Schema.Coproduct[a]
//    final override type Of <: Schema.Coproduct[?]
//
//    def discriminator: Discriminator
//    def discriminator(f: Discriminator => Discriminator): Schema.Coproduct[A]
//    final def discriminator(value: Discriminator): Schema.Coproduct[A] = discriminator(_ => value)
//
//    final override def optional: Schema.Coproduct[Option[A]] = Schema.Coproduct.Optional(this)
//
//    final override def ivalidate[B >: A, C](validation: Validation[B, C])(g: C => B): Schema.Coproduct[C] =
//      Coproduct.Validate(this, validation, g)
//
//    final def orElse[B](coproduct: Schema.Coproduct[B]): Schema.Coproduct[A + B] =
//      Coproduct.OrElse(this, coproduct, None, Discriminator.Default)
//
//    final def :+[B, C](branch: Branch[B, C]): Schema.Coproduct[A + C] = orElse(branch.toCoproduct)
//    final def +:[B, C](branch: Branch[B, C]): Schema.Coproduct[C + A] = branch.toCoproduct.orElse(this)
//
//    final def to[B] /*(using evidence: Evidence.Coproduct.Aux[B, A])*/: Schema.Coproduct[B] = ???
////      imap(evidence.from)(evidence.to)
//
//  object Coproduct:
//    final case class Root[A, B](
//        branch: Branch[A, B],
//        description: Option[String],
//        discriminator: Discriminator
//    ) extends Coproduct[B]:
//      override def constraints: Chain[Constraint] = Chain.empty
//      override def isOptional: Boolean = branch.isOptional
//      override def description(f: Option[String] => Option[String]): Coproduct[B] = copy(description = f(description))
//      override def discriminator(f: Discriminator => Discriminator): Coproduct[B] =
//        copy(discriminator = f(discriminator))
//
//    final case class Optional[A](self: Coproduct[A]) extends Coproduct[Option[A]]:
//      override def constraints: Chain[Constraint] = self.constraints
//      override def isOptional: Boolean = true
//      override def discriminator: Discriminator = self.discriminator
//      override def discriminator(f: Discriminator => Discriminator): Coproduct[Option[A]] =
//        copy(self = self.discriminator(f))
//      override def description: Option[String] = self.description
//      override def description(f: Option[String] => Option[String]): Coproduct[Option[A]] =
//        copy(self = self.description(f))
//
//    final case class Validate[A, B](self: Schema.Coproduct[A], validation: Validation[A, B], g: B => A)
//        extends Schema.Coproduct[B]:
//      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
//      override def isOptional: Boolean = self.isOptional
//      override def discriminator: Discriminator = self.discriminator
//      override def discriminator(f: Discriminator => Discriminator): Coproduct[B] = ???
//      override def description: Option[String] = self.description
//      override def description(f: Option[String] => Option[String]): Coproduct[B] = ???
//
//    final case class OrElse[A, B](
//        left: Schema.Coproduct[A],
//        right: Schema.Coproduct[B],
//        description: Option[String],
//        discriminator: Discriminator
//    ) extends Schema.Coproduct[A + B]:
//      override def constraints: Chain[Constraint] = Chain.empty
//      override def isOptional: Boolean = left.isOptional && right.isOptional
//      override def discriminator(f: Discriminator => Discriminator): Coproduct[A + B] =
//        copy(discriminator = f(discriminator))
//      override def description(f: Option[String] => Option[String]): Coproduct[A + B] =
//        copy(description = f(description))
//
//    def apply[A, B](branch: Branch[A, B]): Schema.Coproduct[B] = Root(branch, None, Discriminator.Default)
//
//  sealed abstract class Dictionary[+A] extends Schema[A]:
//    final override type Self[a] = Dictionary[a]
//
//    final override def optional: Dictionary[Option[A]] = Dictionary.Optional(this)
//
//    final override def ivalidate[B >: A, C](validation: Validation[B, C])(g: C => B): Dictionary[C] =
//      Dictionary.Validate(this, validation, g)
//
//  object Dictionary:
//    final case class Root[A, B](
//        key: Schema.Value[A],
//        value: Schema[B],
//        description: Option[String]
//    ) extends Dictionary[VectorMap[A, B]]:
//      override def constraints: Chain[Constraint] = Chain.empty
//      override def isOptional: Boolean = false
//      override def description(f: Option[String] => Option[String]): Dictionary[VectorMap[A, B]] =
//        copy(description = f(description))
//
//    final case class Optional[A](self: Dictionary[A]) extends Dictionary[Option[A]]:
//      override def constraints: Chain[Constraint] = self.constraints
//      override def isOptional: Boolean = true
//      override def description: Option[String] = self.description
//      override def description(f: Option[String] => Option[String]): Dictionary[Option[A]] =
//        copy(self = self.description(f))
//
//    final case class Validate[A, B](self: Dictionary[A], validation: Validation[A, B], g: B => A) extends Dictionary[B]:
//      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
//      override def isOptional: Boolean = self.isOptional
//      override def description: Option[String] = self.description
//      override def description(f: Option[String] => Option[String]): Dictionary[B] = copy(self = self.description(f))
//
//    def apply[A, B](key: Schema.Value[A], value: Schema[B]): Dictionary[VectorMap[A, B]] = Root(key, value, None)
//
//  sealed abstract class Dynamic[+A] extends Schema[A]:
//    final override type Self[+a] = Schema.Dynamic[a]
//    final override type Of <: Schema.Dynamic[?]
//
//    override def optional: Schema.Dynamic[Option[A]] = Dynamic.Optional(this)
//
//    override def ivalidate[B >: A, C](validation: Validation[B, C])(g: C => B): Schema.Dynamic[C] =
//      Dynamic.Validate(this, validation, g)
//
//  object Dynamic:
//    final case class Root(description: Option[String]) extends Schema.Dynamic[Data.Value]:
//      override def constraints: Chain[Constraint] = Chain.empty
//      override def isOptional: Boolean = false
//      override def description(f: Option[String] => Option[String]): Dynamic[Data.Value] = ???
//
//    final case class Optional[A](self: Schema.Dynamic[A]) extends Schema.Dynamic[Option[A]]:
//      override def constraints: Chain[Constraint] = self.constraints
//      override def isOptional: Boolean = true
//      override def description: Option[String] = self.description
//      override def description(f: Option[String] => Option[String]): Dynamic[Option[A]] = ???
//
//    final case class Validate[A, B](self: Schema.Dynamic[A], validation: Validation[A, B], g: B => A)
//        extends Schema.Dynamic[B]:
//      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
//      override def isOptional: Boolean = self.isOptional
//      override def description: Option[String] = self.description
//      override def description(f: Option[String] => Option[String]): Dynamic[B] = ???
//
//    val Value: Dynamic[Data.Value] = Root(None)
//
//  sealed abstract class Enumeration[+A] extends Schema.Value[A]:
//    final override type Self[+a] = Schema.Enumeration[a]
//    final override type Of <: Schema.Enumeration[?]
//
//    final override def optional: Schema.Enumeration[Option[A]] = Enumeration.Optional(this)
//
//    final override def ivalidate[B >: A, C](validation: Validation[B, C])(g: C => B): Schema.Enumeration[C] =
//      Enumeration.Validate(this, validation, g)
//
//  object Enumeration:
//    final case class Root[A, B](
//        schema: Schema.Value[A],
//        mapping: Mapping[B, A],
//        description: Option[String]
//    ) extends Schema.Enumeration[B]:
//      override def constraints: Chain[Constraint] = Chain.empty
//      override def isOptional: Boolean = false
//      override def description(f: Option[String] => Option[String]): Schema.Enumeration[B] =
//        copy(description = f(description))
//
//    final case class Optional[A](self: Schema.Enumeration[A]) extends Schema.Enumeration[Option[A]]:
//      override def constraints: Chain[Constraint] = self.constraints
//      override def isOptional: Boolean = true
//      override def description: Option[String] = self.description
//      override def description(f: Option[String] => Option[String]): Schema.Enumeration[Option[A]] =
//        copy(self = self.description(f))
//
//    final case class Validate[A, B](self: Enumeration[A], validation: Validation[A, B], g: B => A)
//        extends Enumeration[B]:
//      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
//      override def isOptional: Boolean = self.isOptional
//      override def description: Option[String] = self.description
//      override def description(f: Option[String] => Option[String]): Enumeration[B] = copy(self = self.description(f))
//
//    def apply[A, B](schema: Schema.Value[A], mapping: Mapping[B, A]): Enumeration[B] = Root(schema, mapping, None)
//
//  sealed abstract class Primitive[+A] extends Schema.Value[A]:
//    final override type Self[+a] = Schema.Primitive[a]
//    final override type Of <: Schema.Primitive[?]
//
//    def format: Option[String]
//    def format(f: Option[String] => Option[String]): Schema.Primitive[A]
//    final def format(value: Option[String]): Schema.Primitive[A] = format(_ => value)
//    final def format(value: String): Schema.Primitive[A] = format(Some(value))
//
//    final override def optional: Schema.Primitive[Option[A]] = Primitive.Optional(this)
//
//    final override def ivalidate[B >: A, C](validation: Validation[B, C])(g: C => B): Schema.Primitive[C] =
//      Primitive.Validate(this, validation, g)
//
//  object Primitive:
//    final case class Root[A](tpe: Type[A], description: Option[String], format: Option[String]) extends Primitive[A]:
//      override def isOptional: Boolean = false
//      override def constraints: Chain[Constraint] = Chain.empty
//      override def description(f: Option[String] => Option[String]): Primitive[A] = copy(description = f(description))
//      override def format(f: Option[String] => Option[String]): Primitive[A] = copy(format = f(format))
//
//    final case class Optional[A](self: Primitive[A]) extends Primitive[Option[A]]:
//      override def constraints: Chain[Constraint] = self.constraints
//      override def isOptional: Boolean = true
//      override def format: Option[String] = self.format
//      override def description: Option[String] = self.description
//      override def description(f: Option[String] => Option[String]): Primitive[Option[A]] =
//        copy(self = self.description(f))
//      override def format(f: Option[String] => Option[String]): Primitive[Option[A]] = copy(self = self.format(f))
//
//    final case class Validate[A, B](self: Primitive[A], validation: Validation[A, B], g: B => A) extends Primitive[B]:
//      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
//      override def isOptional: Boolean = self.isOptional
//      override def format: Option[String] = self.format
//      override def description: Option[String] = self.description
//      override def format(f: Option[String] => Option[String]): Primitive[B] = copy(self = self.format(f))
//      override def description(f: Option[String] => Option[String]): Primitive[B] = copy(self = self.description(f))
//
//    def apply[A](tpe: Type[A]): Primitive[A] = Root(tpe, None, None)
//
//  sealed abstract class Product[+A] extends Schema[A]:
//    final override type Self[+a] = Schema.Product[a]
//    final override type Of <: Schema.Product[?]
//
//    final override def optional: Product[Option[A]] = Product.Optional(this)
//
//    final override def ivalidate[B >: A, C](validation: Validation[B, C])(g: C => B): Product[C] =
//      Product.Validate(this, validation, g)
//
//    final def prepend[B](schema: Schema[B]): Product[(B, A)] = Product.Prepend(None, schema, this)
//
//    final def to[B] /*(using evidence: Evidence.Product.Aux[B, A])*/: Product[B] =
//      ??? // imap(evidence.from)(evidence.to)
//
//  object Product:
//    final case class Root[A](description: Option[String], schema: Schema[A]) extends Product[A]:
//      override def constraints: Chain[Constraint] = Chain.empty
//      override def isOptional: Boolean = false
//      override def description(f: Option[String] => Option[String]): Product[A] = copy(description = f(description))
//
//    final case class Optional[A](self: Product[A]) extends Product[Option[A]]:
//      override def constraints: Chain[Constraint] = self.constraints
//      override def isOptional: Boolean = true
//      override def description: Option[String] = self.description
//      override def description(f: Option[String] => Option[String]): Product[Option[A]] =
//        copy(self = self.description(f))
//
//    final case class Validate[A, B](self: Product[A], validation: Validation[A, B], g: B => A) extends Product[B]:
//      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
//      override def isOptional: Boolean = self.isOptional
//      override def description: Option[String] = self.description
//      override def description(f: Option[String] => Option[String]): Product[B] = copy(self = self.description(f))
//
//    final case class Prepend[A, B](
//        description: Option[String],
//        schema: Schema[A],
//        self: Product[B]
//    ) extends Product[(A, B)]:
//      override def constraints: Chain[Constraint] = Chain.empty
//      override def isOptional: Boolean = schema.isOptional && self.isOptional
//      override def description(f: Option[String] => Option[String]): Product[(A, B)] =
//        copy(description = f(description))
//
//    def apply[A](schema: Schema[A]): Schema.Product[A] = Root(None, schema)
//
//  sealed abstract class Record[+A] extends Schema[A]:
//    final override type Self[+a] = Schema.Record[a]
//    final override type Of = Schema.Record[?]
//
//    def nulls: Null
//    def nulls(f: Null => Null): Record[A]
//    final def nulls(value: Null): Record[A] = nulls(_ => value)
//
//    final override def optional: Record[Option[A]] = Record.Optional(this)
//
//    final override def ivalidate[B >: A, C](validation: Validation[B, C])(g: C => B): Record[C] =
//      Record.Validate(this, validation, g)
//
//    final def zip[B](record: Record[B]): Record[(A, B)] = Record.Zip(this, record, None, Null.Default)
//
//    final def to[B] /*(using evidence: Evidence.Product.Aux[B, A])*/: Record[B] =
//      ??? // imap(evidence.from)(evidence.to)
//
//  object Record extends ToRecordOps:
//    final case class Empty(description: Option[String], nulls: Null) extends Record[Unit]:
//      override def constraints: Chain[Constraint] = Chain.empty
//      override def isOptional: Boolean = false
//      override def description(f: Option[String] => Option[String]): Record[Unit] = copy(description = f(description))
//      override def nulls(f: Null => Null): Record[Unit] = copy(nulls = f(nulls))
//
//    final case class Root[A, B](field: Field[A, B], description: Option[String], nulls: Null) extends Record[B]:
//      override def constraints: Chain[Constraint] = Chain.empty
//      override def isOptional: Boolean = false
//      override def nulls(f: Null => Null): Record[B] = ???
//      override def description(f: Option[String] => Option[String]): Record[B] = ???
//
//    final case class Zip[A, B](left: Record[A], right: Record[B], description: Option[String], nulls: Null)
//        extends Record[(A, B)]:
//      override def constraints: Chain[Constraint] = Chain.empty
//      override def isOptional: Boolean = left.isOptional && right.isOptional
//      override def description(f: Option[String] => Option[String]): Record[(A, B)] = copy(description = f(description))
//      override def nulls(f: Null => Null): Record[(A, B)] = copy(nulls = f(nulls))
//
//    final case class Optional[A](self: Record[A]) extends Record[Option[A]]:
//      override def constraints: Chain[Constraint] = self.constraints
//      override def isOptional: Boolean = true
//      override def nulls: Null = self.nulls
//      override def nulls(f: Null => Null): Record[Option[A]] = ???
//      override def description: Option[String] = self.description
//      override def description(f: Option[String] => Option[String]): Record[Option[A]] = ???
//
//    final case class Validate[A, B](self: Record[A], validation: Validation[A, B], g: B => A) extends Record[B]:
//      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
//      override def isOptional: Boolean = self.isOptional
//      override def nulls: Null = self.nulls
//      override def nulls(f: Null => Null): Record[B] = ???
//      override def description: Option[String] = self.description
//      override def description(f: Option[String] => Option[String]): Record[B] = ???
//
//    def apply[A, B](field: Field[A, B]): Record[B] = Root(field, None, Null.Default)
