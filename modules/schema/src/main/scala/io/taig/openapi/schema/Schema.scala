package io.taig.openapi.schema

import cats.data.{Chain, Validated}
import io.taig.openapi.OpenApi
import io.taig.validation.{Constraint, Validation}

abstract class Schema[A]:
  self =>
  type Self[a] <: Schema[a] { type Self[a] = self.Self[a] }
  type Codec <: OpenApi
  type Metadata[a] <: Schema.Metadata[a] { type Self[a] <: Metadata[a] }

  abstract class Attribute[B]:
    def value: Option[B]
    protected def update(f: Option[B] => Option[B]): Metadata[A]
    final def modify(f: Option[B] => Option[B]): Self[A] = self.copy(update(f))
    def set(value: Option[B]): Self[A] = modify(_ => value)
    def as(value: B): Self[A] = modify(_ => Some(value))
    def clear: Self[A] = modify(_ => None)

  def metadata: Metadata[A]

  def copy(metadata: Metadata[A]): Self[A]

  object constraints:
    def value: Chain[Constraint[OpenApi]] = metadata.constraints
    def append(constraints: Chain[Constraint[OpenApi]]): Self[A] = self.copy(metadata.append(constraints))

  object description extends Attribute[String]:
    override def value: Option[String] = metadata.description
    override protected def update(f: Option[String] => Option[String]): Metadata[A] =
      metadata.updated(f(value), metadata.example)

  object example extends Attribute[A]:
    override def value: Option[A] = metadata.example
    override protected def update(f: Option[A] => Option[A]): Metadata[A] =
      metadata.updated(metadata.description, f(value))

  def ivalidate[B](validation: Validation[A, A, A, B])(g: B => A): Self[B] { type Codec = self.Codec }

  final def validate(validation: Validation[A, A, A, Unit]): Self[A] { type Codec = self.Codec } =
    ivalidate(validation.tap)(identity)

  final def imap[B](f: A => B)(g: B => A): Self[B] { type Codec = self.Codec } =
    ivalidate(Validation.lift(f))(g)

  def decode(openapi: OpenApi): Validated[Violations, A]

  def encode(a: A): Codec

  final def optional: Optional.Of[Option[A], Codec] = Optional(this)

object Schema:
  type Of[A, B <: OpenApi] = Schema[A] { type Codec = B }

  trait Metadata[A]:
    type Self[a] <: Schema.Metadata[a]
    def constraints: Chain[Constraint[OpenApi]]
    def description: Option[String]
    def example: Option[A]
    def map[B](f: A => B): Self[B]
    def flatMap[B](f: A => Option[B]): Self[B]
    def updated(description: Option[String], example: Option[A]): Self[A]
    def append(constraints: Chain[Constraint[OpenApi]]): Self[A]
