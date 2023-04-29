package io.taig.openapi.schema

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.validation.{Constraint, Validation}
import io.taig.openapi.{schema, OpenApi}

abstract class Primitive[A] extends Value[A]:
  self =>

  override type Self[a] = Primitive[a]

  def format: Option[String]
  def modifyFormat(f: Option[String] => Option[String]): Primitive[A]
  final def setFormat(format: Option[String]): Primitive[A] = modifyFormat(_ => format)
  final def withFormat(format: String): Primitive[A] = setFormat(Some(format))
  final def withoutFormat: Primitive[A] = setFormat(None)

  def tpe: Type[?]

  override def ivalidate[B](validation: Validation[A, A, A, B])(g: B => A): Primitive[B] =
    Primitive.Validate(this, validation, g, example.flatMap(validation.run(_).toOption))

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

  final private case class Validate[A, B](
      primitive: Primitive[A],
      validation: Validation[A, A, A, B],
      g: B => A,
      example: Option[B]
  ) extends Primitive[B]:
    override def constraints: Chain[Constraint[OpenApi]] =
      primitive.constraints ++ validation.constraints.map(_.map(primitive.encode))
    override def description: Option[String] = primitive.description
    override def format: Option[String] = primitive.format
    override def tpe: Type[?] = primitive.tpe
    override def modifyFormat(f: Option[String] => Option[String]): Primitive[B] =
      copy(primitive = primitive.modifyFormat(f))
    override def modifyDescription(f: Option[String] => Option[String]): Primitive[B] =
      copy(primitive = primitive.modifyDescription(f))
    override def modifyExample(f: Option[B] => Option[B]): Primitive[B] = copy(example = f(example))
    override def decode(openapi: OpenApi): Validated[Violations, B] =
      primitive.decode(openapi).andThen(andThenValidate(validation, primitive.encode))
    override def encode(b: B): OpenApi.Primitive = primitive.encode(g(b))
    override def parse(value: String): Validated[Violations, B] =
      primitive.parse(value).andThen(andThenValidate(validation, primitive.encode))
    override def render(b: B): String = primitive.render(g(b))

  def apply[A](tpe: Type[A]): Primitive[A] = Root(none, none, none, tpe)
