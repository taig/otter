package io.taig.openapi.schema

import cats.data.{Chain, Validated}
import io.taig.openapi.OpenApi
import io.taig.validation.{Constraint, Validation}

abstract class Schema[A]:
  self =>
  type Self[a] <: Schema[a] { type Self[a] = self.Self[a] }
  type Codec <: OpenApi
  type Metadata[a] <: Schema.Metadata[a] { type Self[a] <: Metadata[a] }

  abstract class Attribute[B](val value: B):
    def updated(f: B => B): Metadata[A]
    final def modify(f: B => B): Self[A] { type Codec = self.Codec } = self.copy(updated(f))
    def set(value: B): Self[A] { type Codec = self.Codec } = modify(_ => value)

  object Attribute:
    abstract class Optional[B](value: Option[B]) extends Attribute[Option[B]](value):
      def as(value: B): Self[A] { type Codec = self.Codec } = modify(_ => Some(value))
      def clear: Self[A] { type Codec = self.Codec } = modify(_ => None)

  def metadata: Metadata[A]

  def copy(metadata: Metadata[A]): Self[A] { type Codec = self.Codec }

  def constraints: Chain[Constraint[OpenApi]]

  object description extends Attribute.Optional[String](metadata.description):
    override def updated(f: Option[String] => Option[String]): Metadata[A] =
      metadata.updated(f(value), metadata.example)

  object example extends Attribute.Optional[A](metadata.example):
    override def updated(f: Option[A] => Option[A]): Metadata[A] =
      metadata.updated(metadata.description, f(value))

  def ivalidate[B](validation: Validation[A, A, A, B])(g: B => A): Self[B] { type Codec = self.Codec }

  final def validate(validation: Validation[A, A, A, Unit]): Self[A] { type Codec = self.Codec } =
    ivalidate(validation.tap)(identity)

  final def imap[B](f: A => B)(g: B => A): Self[B] { type Codec = self.Codec } =
    ivalidate(Validation.lift(f))(g)

  final def const(value: A): Self[Void] = imap(_ => Void)(_ => value)

  def decode(openapi: OpenApi): Validated[Violations, A]

  def encode(a: A): Codec

object Schema:
  type Of[A, B <: OpenApi] = Schema[A] { type Codec = B }

  trait Metadata[A]:
    type Self[a] <: Schema.Metadata[a]
    def description: Option[String]
    def example: Option[A]
    def map[B](f: A => B): Self[B]
    def flatMap[B](f: A => Option[B]): Self[B]
    def updated(description: Option[String], example: Option[A]): Self[A]
