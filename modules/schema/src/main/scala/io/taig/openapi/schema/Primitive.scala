package io.taig.openapi.schema

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.syntax.*
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.validation.{Constraint, Validation}

sealed abstract class Primitive[A](
    val constraints: Chain[Constraint[OpenApi]],
    val default: Option[A],
    val description: Option[String],
    val example: Option[A],
    val format: Option[String],
    val name: Option[String],
    val tpe: Type[?]
) extends Value[A]:
  self =>

  final override type Self[a] = Primitive[a]
  final override type Codec = OpenApi.Primitive

  final def modifyFormat(f: Option[String] => Option[String]): Self[A] =
    copy(default, description, example, f(format), name)
  final def setFormat(format: Option[String]): Primitive[A] = self.modifyFormat(_ => format)
  final def withFormat(format: String): Primitive[A] = setFormat(format.some)
  final def withoutFormat: Primitive[A] = setFormat(none)

  final def copy(
      default: Option[A],
      description: Option[String],
      example: Option[A],
      format: Option[String],
      name: Option[String]
  ): Primitive[A] = new Primitive[A](constraints, default, description, example, format, name, tpe):
    export self.{decode, encode}

  final override def copy(
      default: Option[A],
      description: Option[String],
      example: Option[A],
      name: Option[String]
  ): Primitive[A] = copy(default, description, example, format, name)

  override def imap[B](f: A => B)(g: B => A): Primitive[B] =
    new Primitive[B](constraints, default.map(f), description, example.map(f), format, name, tpe):
      override def decode(openapi: OpenApi.Primitive): Validated[Violations, B] = self.decode(openapi).map(f)
      override def encode(b: B): OpenApi.Primitive = self.encode(g(b))

  override def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Primitive[C] = new Primitive[C](
    constraints ++ validation.constraints.map(_.map(_.asOpenApi)),
    default.flatMap(validation.run(_).toOption),
    description,
    example.flatMap(validation.run(_).toOption),
    format,
    name,
    tpe
  ):
    override def decode(openapi: OpenApi.Primitive): Validated[Violations, C] =
      self.decode(openapi).andThen(andThenValidate(validation, self.encode))
    override def encode(b: C): OpenApi.Primitive = self.encode(g(b))

  final override def decode(openapi: OpenApi): Validated[Violations, A] = openapi match
    case OpenApi.Null               => default.toValid(nonNullViolations("Primitive"))
    case openapi: OpenApi.Primitive => decode(openapi)
    case _                          => typeViolations("Primitive", openapi).invalid

  def decode(openapi: OpenApi.Primitive): Validated[Violations, A]

  override def encode(a: A): OpenApi.Primitive

object Primitive:
  def apply[A](of: Type[A]): Primitive[A] = new Primitive[A](Chain.empty, none, none, none, none, none, of):
    override def decode(openapi: OpenApi.Primitive): Validated[Violations, A] = of.decode(openapi)
    override def encode(a: A): OpenApi.Primitive = of.encode(a)
