package io.taig.otter.schema

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.OpenApi
import io.taig.otter.validation.{Constraint, Validation}

import scala.annotation.targetName

abstract class Schema[A]:
  self =>
  type Self[a] <: Schema[a] { type Self[a] = self.Self[a]; type Properties[a] = self.Properties[a] }
  type Codec <: OpenApi.Value
  type Properties[a] <: Schema.Properties[a] { type Self[a] = self.Properties[a] }

  def properties: Properties[A]
  def copy(properties: Properties[A]): Self[A] { type Codec = self.Codec }

  trait Property[B]:
    def value: B
    def modify(f: B => B): Self[A] { type Codec = self.Codec }
    final def apply(value: B): Self[A] { type Codec = self.Codec } = modify(_ => value)

  object Property:
    trait Optional[B] extends Property[Option[B]]:
      @targetName("as")
      def apply(value: B): Self[A] { type Codec = self.Codec } = apply(Some(value))
      def clear: Self[A] { type Codec = self.Codec } = apply(None)

  trait Copy(update: Properties[A]):
    this: Self[A] =>
    export self.{constraints, isOptional}
    final override def properties: Properties[A] = update

  trait Optional:
    this: Self[Option[A]] =>
    export self.constraints
    final override def properties: self.Properties[Option[A]] = self.properties.map(_.some)
    final override def isOptional: Boolean = true

  trait Validate[B](validation: Validation[A, B]):
    this: Self[B] =>
    export self.isOptional
    final override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
    final override def properties: self.Properties[B] = self.properties.flatMap(validation(_).toOption)

  final def description: Property.Optional[String] = new Property.Optional[String]:
    override def value: Option[String] = properties.description
    override def modify(f: Option[String] => Option[String]): Self[A] { type Codec = Schema.this.Codec } =
      copy(properties.modifyDescription(f))

  final def example: Property.Optional[A] = new Property.Optional[A]:
    override def value: Option[A] = properties.example
    override def modify(f: Option[A] => Option[A]): Self[A] { type Codec = Schema.this.Codec } =
      copy(properties.modifyExample(f))

  def constraints: Chain[Constraint]
  def isOptional: Boolean

  def optional: Self[Option[A]] { type Codec = self.Codec }

  def ivalidate[B](validation: Validation[A, B])(g: B => A): Self[B] { type Codec = self.Codec }
  final def validate(validation: Validation[A, Unit]): Self[A] = ivalidate(validation.tap)(identity)
  final def imap[B](f: A => B)(g: B => A): Self[B] = ivalidate(Validation.lift(f))(g)
  final def const(value: A): Self[Unit] = imap(_ => ())(_ => value)

  final def decode(openapi: OpenApi): Validated[Violations, A] = decode(openapi.asValue)
  def decode(openapi: Option[OpenApi.Value]): Validated[Violations, A]
  def encode(a: A): Option[Codec]

  final def toProduct: Product[A] = Product(this)

object Schema extends ToSchemaOps:
  type Of[A <: OpenApi, B] = Schema[B] { type Codec = A }

  trait Properties[+A]:
    type Self[+a] <: Schema.Properties[a]

    def description: Option[String]
    def modifyDescription(f: Option[String] => Option[String]): Self[A]

    def example: Option[A]
    def modifyExample[B](f: Option[A] => Option[B]): Self[B]

    def flatMap[B](f: A => Option[B]): Self[B]
    final def map[B](f: A => B): Self[B] = flatMap(f(_).some)

  abstract class Value[A] extends Schema[A]:
    self =>
    override type Self[a] <: Value[a] { type Self[a] = self.Self[a]; type Properties[a] = self.Properties[a] }
    override type Codec = OpenApi.Primitive
    def parse(value: Option[String]): Validated[Violations, A]
    def print(a: A): Option[String]
