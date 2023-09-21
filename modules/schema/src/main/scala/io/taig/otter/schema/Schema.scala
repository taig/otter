package io.taig.otter.schema

import cats.data.{Chain, Validated}
import io.taig.otter.{OpenApi, Specification}
import io.taig.otter.validation.{Constraint, Validation}

abstract class Schema[A]:
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

  def specification: Specification.Value
  def modifySpecification(f: Specification.Value => Specification.Value): Self[A]

  final def description(f: Option[String] => Option[String]): Self[A] = modifySpecification(_.modifyDescription(f))
  final def description(value: Option[String]): Self[A] = description(_ => value)
  final def description(value: String): Self[A] = description(Some(value))

  final def example(f: Option[OpenApi] => Option[OpenApi]): Self[A] = modifySpecification(_.modifyExample(f))
  final def example(value: Option[A]): Self[A] = example(_ => value.flatMap(encode))
  final def example(value: A): Self[A] = example(Some(value))

  def constraints: Chain[Constraint]
  def isOptional: Boolean

  def optional: Self[Option[A]]

  def ivalidate[B](validation: Validation[A, B])(g: B => A): Self[B]
  final def validate(validation: Validation[A, Unit]): Self[A] = ivalidate(validation.tap)(identity)
  final def imap[B](f: A => B)(g: B => A): Self[B] = ivalidate(Validation.lift(f))(g)
  final def const(value: A): Self[Unit] = imap(_ => ())(_ => value)

  final def decode(openapi: OpenApi): Validated[Violations, A] = decode(openapi.asValue)
  def decode(openapi: Option[OpenApi.Value]): Validated[Violations, A]
  def encode(a: A): Option[OpenApi.Value]

  final def toProduct: Product[A] = Product(this)

object Schema extends ToSchemaOps:
  abstract class Value[A] extends Schema[A]:
    self =>
    override type Self[a] <: Value[a] { type Self[a] = self.Self[a] }
    override def encode(a: A): Option[OpenApi.Primitive]
    def parse(value: Option[String]): Validated[Violations, A]
    def print(a: A): Option[String]
