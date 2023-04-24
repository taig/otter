package io.taig.openapi.schema

import cats.data.Chain
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

  def metadata: Metadata[A]

  def withMetadata(metadata: Metadata[A]): Self[A]

  def description: Field[String] = Field(
    metadata.description,
    f => withMetadata(metadata.copy(f(metadata.description), metadata.example))
  )

  def example: Field[A] = Field(
    metadata.example,
    f => withMetadata(metadata.copy(metadata.description, f(metadata.example)))
  )

  def imap[B](f: A => B)(g: B => A): Self[B] { type Codec = self.Codec }

  def ivalidate[B, C, D](validation: Validation[B, C, A, D])(g: D => A): Self[C] { type Codec = self.Codec }

// final def optional: Optional.Of[Option[A], Codec] = Optional(this)

object Schema:
  type Of[A, B <: OpenApi] = Schema[A] { type Codec = B }

  trait Metadata[A]:
    type Self[a] <: Schema.Metadata[a]
    def constraints: Chain[Constraint[OpenApi]]
    def description: Option[String]
    def example: Option[A]
    def map[B](f: A => B): Self[B]
    def copy(description: Option[String], example: Option[A]): Self[A]
