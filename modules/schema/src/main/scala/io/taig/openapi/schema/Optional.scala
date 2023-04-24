package io.taig.openapi.schema

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.OpenApi
import io.taig.validation.{Constraint, Validation}

sealed abstract class Optional[A](val metadata: Optional.Metadata[A]) extends Schema[A]:
  self =>
  type Of <: OpenApi
  final override type Self[a] = Optional.Of[a, Of]
  final override type Codec = OpenApi.Null.type | Of
  final override type Metadata[a] = Optional.Metadata[a]

  override def copy(metadata: Optional.Metadata[A]): Optional.Of[A, Of] = new Optional[A](metadata):
    export self.{decode, encode, Of}

  override def ivalidate[B](validation: Validation[A, A, A, B])(g: B => A): Optional.Of[B, Of] =
    new Optional[B](metadata.flatMap(validation.run(_).toOption)):
      export self.Of
      override def decode(openapi: OpenApi): Validated[Violations, B] =
        self.decode(openapi).andThen(andThenValidate(validation, self.encode))
      override def encode(b: B): OpenApi.Null.type | self.Of = self.encode(g(b))

object Optional:
  type Of[A, B <: OpenApi] = Optional[A] { type Of = B }

  final case class Metadata[A](
      constraints: Chain[Constraint[OpenApi]],
      description: Option[String],
      example: Option[A],
      schema: Schema[?]
  ) extends Schema.Metadata[A]:
    override type Self[a] = Optional.Metadata[a]

    override def map[B](f: A => B): Optional.Metadata[B] = copy(example = example.map(f))
    override def flatMap[B](f: A => Option[B]): Optional.Metadata[B] = copy(example = example.flatMap(f))
    override def updated(description: Option[String], example: Option[A]): Optional.Metadata[A] =
      Metadata(constraints, description, example, schema)
    override def append(constraints: Chain[Constraint[OpenApi]]): Optional.Metadata[A] =
      copy(constraints = this.constraints ++ constraints)

  object Metadata:
    def empty[A](schema: Schema[?]): Optional.Metadata[A] = Metadata(Chain.empty, None, None, schema)

  def apply[A, B <: OpenApi](of: Schema.Of[A, B]): Optional.Of[Option[A], B] =
    new Optional[Option[A]](Metadata.empty(of)):
      override type Of = B

      override def decode(openapi: OpenApi): Validated[Violations, Option[A]] = openapi match
        case OpenApi.Null => none[A].valid
        case _            => of.decode(openapi).map(_.some)

      override def encode(a: Option[A]): OpenApi.Null.type | B = a.fold(OpenApi.Null)(of.encode)
