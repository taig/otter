package io.taig.openapi.schema

import cats.Eval
import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.OpenApi
import io.taig.openapi.validation.{Constraint, Validation, Violation}

sealed abstract class Enumeration[A] extends Value[A]:
  self =>
  final override type Self[a] = Enumeration[a]

  def schema: Eval[Value[?]]

  def values: Eval[Set[OpenApi.Primitive]]

  final override def ivalidate[B](validation: Validation[A, A, A, B])(g: B => A): Enumeration[B] =
    Enumeration.Validate(this, validation, g, example.flatMap(validation.run(_).toOption))

  final override def decode(openapi: OpenApi): Validated[Violations, A] = openapi match
    case openapi: OpenApi.Primitive => decode(openapi)
    case _                          => typeViolations("Primitive", openapi).invalid

  def decode(openapi: OpenApi.Primitive): Validated[Violations, A]

object Enumeration:
  final private case class Root[A, B](
      description: Option[String],
      example: Option[B],
      mapping: B => A,
      schema: Eval[Value[A]],
      source: Set[B]
  ) extends Enumeration[B]:
    val lookup: Eval[A => Option[B]] = Eval.later(source.map(b => mapping(b) -> b).toMap.get(_))

    override def constraints: Chain[Constraint[OpenApi]] = Chain.empty
    override def values: Eval[Set[OpenApi.Primitive]] = schema.map(schema => source.map(mapping).map(schema.encode))
    override def modifyDescription(f: Option[String] => Option[String]): Enumeration[B] =
      copy(description = f(description))
    override def modifyExample(f: Option[B] => Option[B]): Enumeration[B] = copy(example = f(example))
    override def decode(openapi: OpenApi.Primitive): Validated[Violations, B] = schema.value
      .decode(openapi)
      .andThen: key =>
        Validated.fromOption(
          lookup.value(key), {
            val references = OpenApi.fromList(values.value.toList)
            val constraint = Constraint("enumeration", references.some)
            Violations.rootNec(Violation(constraint, schema.value.encode(key)))
          }
        )
    override def encode(b: B): OpenApi.Primitive = schema.value.encode(mapping(b))
    override def parse(value: String): Validated[Violations, B] =
      schema.value.parse(value).andThen(a => lookup.value(a).toValid(???))
    override def render(b: B): String = schema.value.render(mapping(b))

  final private case class Validate[A, B](
      enumeration: Enumeration[A],
      validation: Validation[A, A, A, B],
      g: B => A,
      example: Option[B]
  ) extends Enumeration[B]:
    override def constraints: Chain[Constraint[OpenApi]] =
      enumeration.constraints ++ validation.constraints.map(_.map(enumeration.encode))
    override def description: Option[String] = enumeration.description
    override def schema: Eval[Value[?]] = enumeration.schema
    override def values: Eval[Set[OpenApi.Primitive]] = enumeration.values
    override def modifyDescription(f: Option[String] => Option[String]): Enumeration[B] =
      copy(enumeration = enumeration.modifyDescription(f))
    override def modifyExample(f: Option[B] => Option[B]): Enumeration[B] = copy(example = f(example))
    override def decode(openapi: OpenApi.Primitive): Validated[Violations, B] =
      enumeration.decode(openapi).andThen(andThenValidate(validation, enumeration.encode))
    override def encode(b: B): OpenApi.Primitive = enumeration.encode(g(b))
    override def parse(value: String): Validated[Violations, B] =
      enumeration.parse(value).andThen(andThenValidate(validation, enumeration.encode))
    override def render(b: B): String = enumeration.render(g(b))

  def apply[A, B](schema: Eval[Value[A]], values: Set[B], mapping: B => A): Enumeration[B] =
    Root(none, none, mapping, schema, values)
