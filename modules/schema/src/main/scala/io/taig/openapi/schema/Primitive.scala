package io.taig.openapi.schema

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.validation.{Constraint, Validation}
import io.taig.openapi.syntax.*
import io.taig.openapi.{schema, Encoder, OpenApi}

abstract class Primitive[A] extends Value[A]:
  self =>

  override type Self[a] = Primitive[a]

  def format: Option[String]
  def modifyFormat(f: Option[String] => Option[String]): Primitive[A]
  final def setFormat(format: Option[String]): Primitive[A] = modifyFormat(_ => format)
  final def withFormat(format: String): Primitive[A] = setFormat(Some(format))
  final def withoutFormat: Primitive[A] = setFormat(None)

  def tpe: Type[?]

  final override def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Primitive[C] =
    Primitive.Validate(this, validation, g)

object Primitive:
  final private case class Root[A](
      description: Option[String],
      example: Option[A],
      format: Option[String],
      tpe: Type[A]
  ) extends Primitive[A]:
    override def constraints: Chain[Constraint[OpenApi]] = Chain.empty
    override def modifyDescription(f: Option[String] => Option[String]): Primitive[A] =
      copy(description = f(description))
    override def modifyExample(f: Option[A] => Option[A]): Primitive[A] = copy(example = f(example))
    override def modifyFormat(f: Option[String] => Option[String]): Primitive[A] = copy(format = f(format))
    override def decode(openapi: OpenApi): Validated[Violations, A] = openapi match
      case openapi: OpenApi.Primitive => tpe.decode(openapi).leftMap(Violations.rootNec)
      case _                          => typeViolations("Primitive", openapi).invalid
    override def encode(a: A): OpenApi.Primitive = tpe.encode(a)
    override def parse(value: String): Validated[Violations, A] =
      tpe.parse(value).toValid(typeViolations(tpe.show, OpenApi.fromString(value)))
    override def render(a: A): String = tpe.render(a)

  final private case class Validate[A, B: Encoder, C](
      primitive: Primitive[A],
      validation: Validation[B, A, A, C],
      g: C => A
  ) extends Primitive[C]:
    override def constraints: Chain[Constraint[OpenApi]] =
      primitive.constraints ++ validation.constraints.map(_.map(_.asOpenApi))
    override def description: Option[String] = primitive.description
    override def example: Option[C] = primitive.example.flatMap(validation.run(_).toOption)
    override def format: Option[String] = primitive.format
    override def tpe: Type[?] = primitive.tpe
    override def modifyFormat(f: Option[String] => Option[String]): Primitive[C] =
      copy(primitive = primitive.modifyFormat(f))
    override def modifyDescription(f: Option[String] => Option[String]): Primitive[C] =
      copy(primitive = primitive.modifyDescription(f))
    override def modifyExample(f: Option[C] => Option[C]): Primitive[C] =
      copy(primitive = primitive.modifyExample(a => f(a.flatMap(validation.run(_).toOption)).map(g)))
    override def decode(openapi: OpenApi): Validated[Violations, C] =
      primitive.decode(openapi).andThen(andThenValidate(validation, primitive.encode))
    override def encode(b: C): OpenApi.Primitive = primitive.encode(g(b))
    override def parse(value: String): Validated[Violations, C] =
      primitive.parse(value).andThen(andThenValidate(validation, primitive.encode))
    override def render(b: C): String = primitive.render(g(b))

  def apply[A](tpe: Type[A]): Primitive[A] = Root(none, none, none, tpe)
