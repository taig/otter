package io.taig.openapi.schema

import cats.Eval
import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.enumeration.ext.Mapping
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.openapi.syntax.*
import io.taig.openapi.validation.{Constraint, Validation, Violation}

sealed abstract class Enumeration[A] extends Value[A]:
  self =>
  final override type Self[a] = Enumeration[a]

  def schema: Eval[Value[?]]

  def values: Eval[List[OpenApi.Primitive]]

  final override def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Enumeration[C] =
    Enumeration.Validate(this, validation, g)

  final override def decode(openapi: OpenApi): Validated[Violations, A] = openapi match
    case openapi: OpenApi.Primitive => decode(openapi)
    case _                          => typeViolations("Primitive", openapi).invalid

  def decode(openapi: OpenApi.Primitive): Validated[Violations, A]

object Enumeration:
  final private case class Root[A, B](
      description: Option[String],
      example: Option[B],
      mapping: Mapping[B, A],
      schema: Eval[Value[A]]
  ) extends Enumeration[B]:
    override def constraints: Chain[Constraint[OpenApi]] = Chain.empty
    override def values: Eval[List[OpenApi.Primitive]] =
      schema.map(schema => mapping.values.map(mapping.inj).map(schema.encode))
    override def modifyDescription(f: Option[String] => Option[String]): Enumeration[B] =
      copy(description = f(description))
    override def modifyExample(f: Option[B] => Option[B]): Enumeration[B] = copy(example = f(example))
    override def decode(openapi: OpenApi.Primitive): Validated[Violations, B] = schema.value
      .decode(openapi)
      .andThen: key =>
        mapping.prj(key).toValid {
          val references = OpenApi.fromList(values.value.toList)
          val constraint = Constraint("enumeration", references.some)
          Violations.rootNec(Violation(constraint, schema.value.encode(key)))
        }
    override def encode(b: B): OpenApi.Primitive = schema.value.encode(mapping(b))
    override def parse(value: String): Validated[Violations, B] = schema.value
      .parse(value)
      .andThen: a =>
        mapping.prj(a).toValid {
          val references = OpenApi.fromList(values.value.toList)
          Violations.rootNec(Constraint("enumeration", references.some).toViolation(schema.value.encode(a)))
        }

    override def render(b: B): String = schema.value.render(mapping(b))

  final private case class Validate[A, B: Encoder, C](
      enumeration: Enumeration[A],
      validation: Validation[B, A, A, C],
      g: C => A
  ) extends Enumeration[C]:
    export enumeration.values
    override def constraints: Chain[Constraint[OpenApi]] =
      enumeration.constraints ++ validation.constraints.map(_.map(_.asOpenApi))
    override def description: Option[String] = enumeration.description
    override def example: Option[C] = enumeration.example.flatMap(validation.run(_).toOption)
    override def schema: Eval[Value[?]] = enumeration.schema
    override def modifyDescription(f: Option[String] => Option[String]): Enumeration[C] =
      copy(enumeration = enumeration.modifyDescription(f))
    override def modifyExample(f: Option[C] => Option[C]): Enumeration[C] =
      copy(enumeration = enumeration.modifyExample(a => f(a.flatMap(validation.run(_).toOption)).map(g)))
    override def decode(openapi: OpenApi.Primitive): Validated[Violations, C] =
      enumeration.decode(openapi).andThen(applyValidation(validation, enumeration.encode))
    override def encode(b: C): OpenApi.Primitive = enumeration.encode(g(b))
    override def parse(value: String): Validated[Violations, C] =
      enumeration.parse(value).andThen(applyValidation(validation, enumeration.encode))
    override def render(b: C): String = enumeration.render(g(b))

  def apply[A, B](schema: Eval[Value[A]], mapping: Mapping[B, A]): Enumeration[B] = Root(none, none, mapping, schema)
