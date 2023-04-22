package io.taig.openapi.schema

import cats.Eval
import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.openapi.syntax.*
import io.taig.screening.{Constraint, Validation}

sealed abstract class Optional[A](
    val constraints: Chain[Constraint[OpenApi]],
    val description: Option[String],
    val example: Option[A],
    val name: Option[String],
    val schema: Schema[?]
) extends Schema[A]:
  self =>
  type Of <: OpenApi
  final override type Self[a] = Optional.Of[a, Of]
  final override type Codec = OpenApi.Null.type | Of

  override def copy(description: Option[String], example: Option[A], name: Option[String]): Optional.Of[A, Of] =
    new Optional[A](constraints, description, example, name, schema):
      export self.{decode, encode, Of}

  override def imap[B](f: A => B)(g: B => A): Optional.Of[B, Of] =
    new Optional[B](constraints, description, example.map(f), name, schema):
      export self.Of
      override def decode(openapi: OpenApi): Validated[Violations, B] = self.decode(openapi).map(f)
      override def encode(b: B): OpenApi.Null.type | self.Of = self.encode(g(b))

  override def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Optional.Of[C, Of] =
    new Optional[C](
      constraints ++ validation.constraints.map(_.map(_.asOpenApi)),
      description,
      example.flatMap(validation.run(_).toOption),
      name,
      schema
    ):
      export self.Of
      override def decode(openapi: OpenApi): Validated[Violations, C] =
        self.decode(openapi).andThen(andThenValidate(validation, self.encode))
      override def encode(b: C): OpenApi.Null.type | self.Of = self.encode(g(b))

  override def decode(openapi: OpenApi): Validated[Violations, A]
  override def encode(a: A): OpenApi.Null.type | Of

object Optional:
  type Of[A, B <: OpenApi] = Optional[A] { type Of = B }

  def apply[A, B <: OpenApi](of: Schema.Of[A, B]): Optional.Of[Option[A], B] =
    new Optional[Option[A]](Chain.empty, none, none, none, of):
      override type Of = B

      override def decode(openapi: OpenApi): Validated[Violations, Option[A]] = openapi match
        case OpenApi.Null => none[A].valid
        case _            => of.decode(openapi).map(_.some)

      override def encode(a: Option[A]): OpenApi.Null.type | B = a.fold(OpenApi.Null)(of.encode)
