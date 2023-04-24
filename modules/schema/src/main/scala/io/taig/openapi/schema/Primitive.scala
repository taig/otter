package io.taig.openapi.schema

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.{schema, OpenApi}
import io.taig.validation.{Constraint, Validation}

abstract class Primitive[A](val metadata: Primitive.Metadata[A]) extends Value[A]:
  self =>

  override type Self[a] = Primitive[a]
  override type Codec = OpenApi.Primitive
  override type Metadata[a] = Primitive.Metadata[a]

  def format: Field[String] = Field(metadata.format, f => self.copy(metadata.copy(format = f(metadata.format))))
  def tpe: Type[?] = metadata.tpe

  override def copy(metadata: Primitive.Metadata[A]): Primitive[A] = new Primitive[A](metadata):
    export self.{decode, encode}

  // TODO are the types too restrictive???
  override def ivalidate[B](validation: Validation[A, A, A, B])(g: B => A): Primitive[B] =
    new Primitive[B](metadata.flatMap(validation.run(_).toOption)):
      override def decode(openapi: OpenApi.Primitive): Validated[Violations, B] =
        self.decode(openapi).andThen(andThenValidate(validation, self.encode))
      override def encode(b: B): OpenApi.Primitive = self.encode(g(b))

  final override def decode(openapi: OpenApi): Validated[Violations, A] = openapi match
    case OpenApi.Null               => default.value.toValid(nonNullViolations("Primitive"))
    case openapi: OpenApi.Primitive => decode(openapi)
    case _                          => typeViolations("Primitive", openapi).invalid

  def decode(openapi: OpenApi.Primitive): Validated[Violations, A]

  def encode(a: A): OpenApi.Primitive

object Primitive:
  final case class Metadata[A](
      constraints: Chain[Constraint[OpenApi]],
      default: Option[A],
      description: Option[String],
      example: Option[A],
      format: Option[String],
      tpe: Type[?]
  ) extends Value.Metadata[A]:
    override type Self[a] = Primitive.Metadata[a]
    override def map[B](f: A => B): Primitive.Metadata[B] = copy(default = default.map(f), example = example.map(f))
    override def flatMap[B](f: A => Option[B]): Metadata[B] =
      copy(default = default.flatMap(f), example = example.flatMap(f))
    override def updated(default: Option[A], description: Option[String], example: Option[A]): Metadata[A] =
      Metadata(constraints, default, description, example, format, tpe)
    override def append(constraints: Chain[Constraint[OpenApi]]): Metadata[A] =
      Metadata(this.constraints ++ constraints, default, description, example, format, tpe)

  object Metadata:
    def empty[A](tpe: Type[A]): Primitive.Metadata[A] = Metadata(Chain.empty, None, None, None, None, tpe)

  def apply[A](of: Type[A]): Primitive[A] = new Primitive[A](Metadata.empty(of)):
    override def decode(openapi: OpenApi.Primitive): Validated[Violations, A] = of.decode(openapi)
    override def encode(a: A): OpenApi.Primitive = of.encode(a)
