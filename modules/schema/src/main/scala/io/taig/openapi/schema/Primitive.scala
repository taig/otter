package io.taig.openapi.schema

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.{schema, OpenApi}
import io.taig.validation.{Constraint, Validation}

abstract class Primitive[A](
    val constraints: Chain[Constraint[OpenApi]],
    val metadata: Primitive.Metadata[A],
    val tpe: Type[?]
) extends Value[A]:
  self =>

  override type Self[a] = Primitive[a]
  override type Codec = OpenApi.Primitive
  override type Metadata[a] = Primitive.Metadata[a]

  object format extends Attribute.Optional[String](metadata.format):
    override protected def update(f: Option[String] => Option[String]): Primitive.Metadata[A] =
      metadata.copy(format = f(value))

  override def copy(metadata: Primitive.Metadata[A]): Primitive[A] = new Primitive[A](constraints, metadata, tpe):
    export self.{decode, encode}

//  def optional2: Primitive[Option[A]] = new Primitive[Option[A]](???, ???, ???):
//    override type Codec = self.Codec | OpenApi.Null.type
//
//    override def decode(openapi: OpenApi.Primitive): Validated[Violations, Option[A]] = ???
//
//    override def encode(a: Option[A]): OpenApi.Primitive = ???

  // TODO are the types too restrictive???
  override def ivalidate[B](validation: Validation[A, A, A, B])(g: B => A): Primitive[B] =
    new Primitive[B](
      constraints ++ validation.constraints.map(_.map(???)),
      metadata.flatMap(validation.run(_).toOption),
      tpe
    ):
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
      default: Option[A],
      description: Option[String],
      example: Option[A],
      format: Option[String]
  ) extends Value.Metadata[A]:
    override type Self[a] = Primitive.Metadata[a]
    override def map[B](f: A => B): Primitive.Metadata[B] = copy(default = default.map(f), example = example.map(f))
    override def flatMap[B](f: A => Option[B]): Metadata[B] =
      copy(default = default.flatMap(f), example = example.flatMap(f))
    override def updated(default: Option[A], description: Option[String], example: Option[A]): Metadata[A] =
      Metadata(default, description, example, format)

  object Metadata:
    def empty[A]: Primitive.Metadata[A] = Metadata(None, None, None, None)

  def apply[A](of: Type[A]): Primitive[A] = new Primitive[A](Chain.empty, Metadata.empty, of):
    override def decode(openapi: OpenApi.Primitive): Validated[Violations, A] = of.decode(openapi)
    override def encode(a: A): OpenApi.Primitive = of.encode(a)
