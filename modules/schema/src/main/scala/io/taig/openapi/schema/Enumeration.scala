package io.taig.openapi.schema

import cats.Eval
import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.openapi.syntax.*
import io.taig.validation.syntax.*
import io.taig.validation.{identifiers, Constraint, Validation, Violation}

sealed abstract class Enumeration[A](
    val constraints: Chain[Constraint[OpenApi]],
    val default: Option[A],
    val description: Option[String],
    val example: Option[A],
    val name: Option[String],
    val schema: Eval[Schema.Of[?, OpenApi.Primitive]],
    val values: Eval[Set[OpenApi.Primitive]]
) extends Value[A]:
  self =>

  final override type Self[a] = Enumeration[a]
  final override type Codec = OpenApi.Primitive

  override def copy(
      default: Option[A],
      description: Option[String],
      example: Option[A],
      name: Option[String]
  ): Enumeration[A] = new Enumeration[A](constraints, default, description, example, name, schema, values):
    export self.{decode, encode}

  override def imap[B](f: A => B)(g: B => A): Enumeration[B] = new Enumeration[B](
    constraints,
    default.map(f),
    description,
    example.map(f),
    name,
    schema,
    values
  ):
    override def decode(openapi: OpenApi.Primitive): Validated[Violations, B] = self.decode(openapi).map(f)
    override def encode(b: B): OpenApi.Primitive = self.encode(g(b))

  override def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Enumeration[C] =
    new Enumeration[C](
      constraints ++ validation.constraints.map(_.map(_.asOpenApi)),
      default.flatMap(validation.run(_).toOption),
      description,
      example.flatMap(validation.run(_).toOption),
      name,
      schema,
      values
    ):
      override def decode(openapi: OpenApi.Primitive): Validated[Violations, C] =
        self.decode(openapi).andThen(andThenValidate(validation, self.encode))
      override def encode(b: C): OpenApi.Primitive = self.encode(g(b))

  override def decode(openapi: OpenApi): Validated[Violations, A] = openapi match
    case OpenApi.Null               => default.toValid(nonNullViolations("Primitive"))
    case openapi: OpenApi.Primitive => decode(openapi)
    case _                          => typeViolations("Primitive", openapi).invalid

  def decode(openapi: OpenApi.Primitive): Validated[Violations, A]
  override def encode(a: A): OpenApi.Primitive

object Enumeration:
  def apply[A, B](of: Eval[Schema.Of[A, OpenApi.Primitive]], values: Set[B], mapping: B => A): Enumeration[B] =
    val lookup: Eval[A => Option[B]] = Eval.later(values.map(b => mapping(b) -> b).toMap.get(_))
    val openapis = of.map(schema => values.map(mapping).map(schema.encode))

    new Enumeration[B](Chain.empty, none, none, none, none, of, openapis):
      override def decode(openapi: OpenApi.Primitive): Validated[Violations, B] =
        of.value.decode(openapi).andThen { key =>
          Validated.fromOption(
            lookup.value(key), {
              val references = OpenApi.fromList(openapis.value.toList)
              val constraint = identifiers.enumeration.toConstraint(references.some)
              Violations.rootNec(Violation(constraint, of.value.encode(key)))
            }
          )
        }

      override def encode(b: B): OpenApi.Primitive = of.value.encode(mapping(b))
