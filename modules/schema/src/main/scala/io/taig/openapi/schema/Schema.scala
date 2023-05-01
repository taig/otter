package io.taig.openapi.schema

import cats.data.{Chain, Validated}
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.openapi.validation.{Constraint, Validation}

abstract class Schema[A]:
  self =>
  type Self[a] <: Schema[a] { type Self[a] = self.Self[a] }
  type Codec <: OpenApi

  def constraints: Chain[Constraint[OpenApi]]

  def description: Option[String]
  def modifyDescription(f: Option[String] => Option[String]): Self[A]
  final def setDescription(value: Option[String]): Self[A] = modifyDescription(_ => value)
  final def withDescription(value: String): Self[A] = setDescription(Some(value))
  final def withoutDescription: Self[A] = setDescription(None)

  def example: Option[A]
  def modifyExample(f: Option[A] => Option[A]): Self[A]
  final def setExample(example: Option[A]): Self[A] = modifyExample(_ => example)
  final def withExample(example: A): Self[A] = setExample(Some(example))
  final def withoutExample: Self[A] = setExample(None)

  def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Self[C] { type Codec = self.Codec }

  final def validate[B: Encoder](validation: Validation[B, A, A, Unit]): Self[A] { type Codec = self.Codec } =
    ivalidate(validation.tap)(identity)

  final def imap[B](f: A => B)(g: B => A): Self[B] { type Codec = self.Codec } =
    ivalidate(Validation.lift(f))(g)

  final def const(value: A): Self[Void] = imap(_ => Void)(_ => value)

  def decode(openapi: OpenApi): Validated[Violations, A]

  def encode(a: A): Codec

object Schema:
  type Of[A, B <: OpenApi] = Schema[A] { type Codec = B }
