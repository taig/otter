package io.taig.openapi.schema

import cats.data.{Chain, Validated}
import io.taig.openapi.OpenApi
import io.taig.openapi.validation.{Constraint, Validation}

abstract class Schema[A]:
  self =>

  type Self[a] <: Schema[a] { type Self[a] = self.Self[a] }
  type Codec <: OpenApi

  def constraints: Chain[Constraint]

  def description: Property.Optional[Self[A], String]
  def example: Property.Optional[Self[A], A]

  def ivalidate[B](validation: Validation[A, B])(g: B => A): Self[B] { type Codec = self.Codec }
  final def validate[B](validation: Validation[A, Unit]): Self[A] { type Codec = self.Codec } =
    ivalidate(validation.tap)(identity)
  final def imap[B](f: A => B)(g: B => A): Self[B] { type Codec = self.Codec } =
    ivalidate(Validation.lift(f))(g)

  final def const(value: A): Self[Unit] = imap(_ => ())(_ => value)

  def decode(openapi: OpenApi): Validated[Violations, A]
  def encode(a: A): Codec

object Schema:
  abstract class Value[A] extends Schema[A]:
    self =>
    override type Self[a] <: Value[a] { type Self[a] = self.Self[a] }
    final override type Codec = OpenApi.Primitive

    def parse(value: String): Validated[Violations, A]
    def render(a: A): String

  type Of[A, B <: OpenApi] = Schema[A] { type Codec = B }
