package io.taig.openapi.schema

import cats.data.{Chain, Validated}
import io.taig.openapi.OpenApi
import io.taig.validation.{Constraint, Validation}

abstract class Schema[A]:
  self =>
  type Self[a] <: Schema[a] { type Self[a] = self.Self[a] }
  type Codec <: OpenApi
  type Metadata[a] <: Schema.Metadata[a] { type Self[a] <: Metadata[a] }

  final class Field[B](val value: Option[B], val modify: (Option[B] => Option[B]) => Self[A]):
    def set(value: Option[B]): Self[A] = modify(_ => value)
    def as(value: B): Self[A] = modify(_ => Some(value))
    def clear: Self[A] = modify(_ => None)

  final class Constraints(val value: Chain[Constraint[OpenApi]]):
    def append(constrains: Chain[Constraint[OpenApi]]): Self[A] = self.copy(metadata.append(constrains))

  def metadata: Metadata[A]

  def copy(metadata: Metadata[A]): Self[A]

  final def constrains: Constraints = Constraints(metadata.constraints)

  final def description: Field[String] = Field(
    metadata.description,
    f => self.copy(metadata.updated(f(metadata.description), metadata.example))
  )

  final def example: Field[A] = Field(
    metadata.example,
    f => self.copy(metadata.updated(metadata.description, f(metadata.example)))
  )

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
