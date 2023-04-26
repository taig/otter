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

  override type Codec <: OpenApi.Primitive | OpenApi.Null.type
  override type Self[a] = Primitive[a]
  override type Metadata[a] = Primitive.Metadata[a]

  object format extends Attribute.Optional[String](metadata.format):
    override def updated(f: Option[String] => Option[String]): Primitive.Metadata[A] =
      metadata.copy(format = f(value))

  override def copy(metadata: Primitive.Metadata[A]): Primitive.Of[A, Codec] =
    new Primitive[A](constraints, metadata, tpe) { export self.{decode, encode, Codec} }

  override def ivalidate[B](validation: Validation[A, A, A, B])(g: B => A): Primitive.Of[B, Codec] =
    new Primitive[B](
      constraints ++ validation.constraints.map(_.map(self.encode)),
      metadata.flatMap(validation.run(_).toOption),
      tpe
    ):
      override type Codec = self.Codec
      override def decode(openapi: OpenApi): Validated[Violations, B] =
        self.decode(openapi).andThen(andThenValidate(validation, self.encode))
      override def encode(b: B): self.Codec = self.encode(g(b))

  override def optional: Primitive.Of[Option[A], self.Codec | OpenApi.Null.type] =
    new Primitive[Option[A]](constraints, metadata.map(_.some), tpe):
      override type Codec = self.Codec | OpenApi.Null.type
      override def decode(openapi: OpenApi): Validated[Violations, Option[A]] = openapi match
        case OpenApi.Null => none[A].valid
        case openapi      => self.decode(openapi).map(_.some)
      override def encode(a: Option[A]): self.Codec | OpenApi.Null.type = a.fold(OpenApi.Null)(self.encode)

object Primitive:
  type Of[A, B <: OpenApi] = Primitive[A] { type Codec = B }

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

  def apply[A](of: Type[A]): Primitive.Of[A, OpenApi.Primitive] = new Primitive[A](Chain.empty, Metadata.empty, of):
    override type Codec = OpenApi.Primitive
    override def decode(openapi: OpenApi): Validated[Violations, A] = openapi match
      case openapi: OpenApi.Primitive => of.decode(openapi)
      case OpenApi.Null               => nonNullViolations("Primitive").invalid
      case _                          => typeViolations("Primitive", openapi).invalid
    override def encode(a: A): OpenApi.Primitive = of.encode(a)
