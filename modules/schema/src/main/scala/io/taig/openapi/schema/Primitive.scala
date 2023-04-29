package io.taig.openapi.schema

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.{schema, OpenApi}
import io.taig.validation.{Constraint, Validation}

abstract class Primitive[A](
    val constraints: Chain[Constraint[OpenApi]],
    val description: Option[String],
    val example: Option[A],
    val format: Option[String],
    val tpe: Type[?]
) extends Value[A]:
  self =>

  override type Self[a] = Primitive[a]

  final override def modifyDescription(f: Option[String] => Option[String]): Primitive[A] =
    new Primitive[A](constraints, f(description), example, format, tpe) { export self.{decode, encode, parse, render} }

  final override def modifyExample(f: Option[A] => Option[A]): Primitive[A] =
    new Primitive[A](constraints, description, f(example), format, tpe) { export self.{decode, encode, parse, render} }

  override def ivalidate[B](validation: Validation[A, A, A, B])(g: B => A): Primitive[B] =
    new Primitive[B](
      constraints ++ validation.constraints.map(_.map(self.encode)),
      description,
      example.flatMap(validation.run(_).toOption),
      format,
      tpe
    ):
      override def decode(openapi: OpenApi): Validated[Violations, B] = self.decode(openapi).andThen(???)
      override def encode(b: B): OpenApi.Primitive = self.encode(g(b))
      override def parse(value: String): Validated[Violations, B] = self.parse(value).andThen(???)
      override def render(b: B): String = self.render(g(b))

object Primitive:
  def apply[A](of: Type[A]): Primitive[A] = new Primitive[A](Chain.empty, none, none, none, of):
    override def decode(openapi: OpenApi): Validated[Violations, A] = openapi match
      case openapi: OpenApi.Primitive => of.decode(openapi).leftMap(Violations.rootNec)
      case _                          => typeViolations("Primitive", openapi).invalid
    override def encode(a: A): OpenApi.Primitive = of.encode(a)
    override def parse(value: String): Validated[Violations, A] =
      of.parse(value).toValid(typeViolations(tpe.show, OpenApi.fromString(value)))
    override def render(a: A): String = of.render(a)
