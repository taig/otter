package io.taig.openapi.schema

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.{schema, OpenApi}
import io.taig.validation.{Constraint, Validation}

abstract class Primitive[A](
    val constraints: Chain[Constraint[OpenApi]],
    val metadata: Primitive.Metadata[A],
    val tpe: Type[?]
) extends Schema[A]:
  self =>

  final override type Codec = OpenApi.Primitive
  final override type Self[a] = Primitive[a]
  final override type Metadata[a] = Primitive.Metadata[a]

  object format extends Attribute.Optional[String](metadata.format):
    override def updated(f: Option[String] => Option[String]): Primitive.Metadata[A] =
      metadata.copy(format = f(value))

  final override def copy(metadata: Primitive.Metadata[A]): Primitive[A] =
    new Primitive[A](constraints, metadata, tpe) { export self.{decode, encode, parse} }

  final override def ivalidate[B](validation: Validation[A, A, A, B])(g: B => A): Primitive[B] = new Primitive[B](
    constraints ++ validation.constraints.map(_.map(self.encode)),
    metadata.flatMap(validation.run(_).toOption),
    tpe
  ):
    override def decode(openapi: OpenApi): Validated[Violations, B] =
      self.decode(openapi).andThen(andThenValidate(validation, self.encode))
    override def encode(b: B): self.Codec = self.encode(g(b))
    override def parse(value: String): Validated[Violations, B] =
      self.parse(value).andThen(andThenValidate(validation, self.encode))

  def parse(value: String): Validated[Violations, A]

object Primitive:
  final case class Metadata[A](
      description: Option[String],
      example: Option[A],
      format: Option[String]
  ) extends Schema.Metadata[A]:
    override type Self[a] = Primitive.Metadata[a]
    override def map[B](f: A => B): Primitive.Metadata[B] = copy(example = example.map(f))
    override def flatMap[B](f: A => Option[B]): Metadata[B] = copy(example = example.flatMap(f))
    override def updated(description: Option[String], example: Option[A]): Metadata[A] =
      Metadata(description, example, format)

  object Metadata:
    def empty[A]: Primitive.Metadata[A] = Metadata(None, None, None)

  def apply[A](of: Type[A]): Primitive[A] = new Primitive[A](Chain.empty, Metadata.empty, of):
    override def decode(openapi: OpenApi): Validated[Violations, A] = openapi match
      case openapi: OpenApi.Primitive => of.decode(openapi)
      case OpenApi.Null               => nonNullViolations("Primitive").invalid
      case _                          => typeViolations("Primitive", openapi).invalid
    override def encode(a: A): OpenApi.Primitive = of.encode(a)
    override def parse(value: String): Validated[Violations, A] = of.parse(value)
