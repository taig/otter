package io.taig.openapi.schema

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.validation.{Constraint, Validation, Violation}
import io.taig.openapi.{schema, OpenApi}

sealed abstract class Primitive[A] extends Schema.Value[A]:
  final override type Self[a] = Primitive[a]

  def tpe: Type[?]

  def format: Property.Optional[Primitive[A], String]

  final override def decode(openapi: OpenApi): Validated[Violations, A] = openapi match
    case openapi: OpenApi.Primitive => decode(openapi)
    case _                          => Violations.rootNec(Violation.tpe("primitive", openapi)).invalid
  def decode(openapi: OpenApi.Primitive): Validated[Violations, A]

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Primitive[B] =
    Primitive.Validate(this, validation, g)

object Primitive:
  final private case class Root[A](
      _description: Option[String],
      _example: Option[A],
      _format: Option[String],
      tpe: Type[A]
  ) extends Primitive[A]:
    override def constraints: Chain[Constraint] = Chain.empty
    override def description: Property.Optional[Primitive[A], String] =
      Property.Optional(_description, f => copy(_description = f(_description)))
    override def example: Property.Example[Primitive[A], A] =
      ??? // Property.Optional(_example, f => copy(_example = f(_example)))
    override def format: Property.Optional[Primitive[A], String] =
      Property.Optional(_format, f => copy(_format = f(_format)))
    override def decode(openapi: OpenApi.Primitive): Validated[Violations, A] =
      tpe.decode(openapi).toValid(Violations.rootNec(Violation.tpe(tpe.show, openapi)))
    override def encode(a: A): OpenApi.Primitive = tpe.encode(a)
    override def parse(value: String): Validated[Violations, A] =
      tpe.parse(value).toValid(Violations.rootNec(Violation.tpe(tpe.show, OpenApi.fromString(value))))
    override def render(a: A): String = tpe.render(a)

  final private case class Validate[A, B](primitive: Primitive[A], validation: Validation[A, B], g: B => A)
      extends Primitive[B]:
    export primitive.tpe
    override def constraints: Chain[Constraint] = primitive.constraints ++ validation.constraints
    override def description: Property.Optional[Primitive[B], String] =
      Property.Optional(primitive.description, fa => copy(primitive = fa))
    override def example: Property.Example[Primitive[B], B] =
      ??? // Property.Optional(primitive.example, fa => copy(primitive = fa), validation, g)
    override def format: Property.Optional[Primitive[B], String] =
      Property.Optional(primitive.format, fa => copy(primitive = fa))
    override def decode(openapi: OpenApi.Primitive): Validated[Violations, B] =
      primitive.decode(openapi).andThen(validation(_).leftMap(Violations.root))
    override def encode(b: B): OpenApi.Primitive = primitive.encode(g(b))
    override def parse(value: String): Validated[Violations, B] =
      primitive.parse(value).andThen(validation(_).leftMap(Violations.root))
    override def render(b: B): String = primitive.render(g(b))

  def apply[A](tpe: Type[A]): Primitive[A] = Root(None, None, None, tpe)
