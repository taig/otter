package io.taig.otter.schema

import cats.data.{Chain, Validated}
import io.taig.otter.OpenApi
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
