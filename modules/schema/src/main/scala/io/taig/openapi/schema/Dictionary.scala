package io.taig.openapi.schema

import cats.Eval
import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.openapi.syntax.*
import io.taig.validation.{Constraint, Validation}

sealed abstract class Dictionary[A](
    val constraints: Chain[Constraint[OpenApi]],
    val default: Option[A],
    val description: Option[String],
    val example: Option[A],
    val key: Eval[Schema.Of[?, OpenApi.Primitive]],
    val name: Option[String],
    val schema: Eval[Schema[?]]
) extends Value[A]:
  self =>

  final override type Self[a] = Dictionary[a]
  final override type Codec = OpenApi.Object

  override def copy(
      default: Option[A],
      description: Option[String],
      example: Option[A],
      name: Option[String]
  ): Dictionary[A] =
    new Dictionary[A](constraints, default, description, example, key, name, schema):
      export self.{decode, encode}

  override def imap[B](f: A => B)(g: B => A): Dictionary[B] =
    new Dictionary[B](constraints, default.map(f), description, example.map(f), key, name, schema):
      override def decode(openapi: OpenApi.Object): Validated[Violations, B] = self.decode(openapi).map(f)
      override def encode(b: B): OpenApi.Object = self.encode(g(b))

  override def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Dictionary[C] =
    new Dictionary[C](
      constraints ++ validation.constraints.map(_.map(_.asOpenApi)),
      default.flatMap(validation.run(_).toOption),
      description,
      example.flatMap(validation.run(_).toOption),
      key,
      name,
      schema
    ):
      override def decode(openapi: OpenApi.Object): Validated[Violations, C] =
        self.decode(openapi).andThen(andThenValidate(validation, self.encode))
      override def encode(b: C): OpenApi.Object = self.encode(g(b))

  final override def decode(openapi: OpenApi): Validated[Violations, A] = openapi match
    case OpenApi.Null            => decode(OpenApi.Object.Empty).orElse(default.toValid(nonNullViolations("Object")))
    case openapi: OpenApi.Object => decode(openapi)
    case _                       => typeViolations("Object", openapi).invalid

  def decode(openapi: OpenApi.Object): Validated[Violations, A]
  override def encode(a: A): OpenApi.Object

object Dictionary:
  def apply[A, B](ofKey: Eval[Schema.Of[A, OpenApi.Primitive]], ofSchema: Eval[Schema[B]]): Dictionary[Map[A, B]] =
    new Dictionary[Map[A, B]](Chain.empty, none, none, none, ofKey, none, ofSchema):
      override def decode(openapi: OpenApi.Object): Validated[Violations, Map[A, B]] =
        openapi.toMap.toSeq
          .traverse { case (key, value) =>
            (ofKey.value.decode(OpenApi.fromString(key)), ofSchema.value.decode(value)).tupled
              .leftMap(_.modifyHistory(key /: _))
          }
          .map(_.toMap)

      override def encode(abs: Map[A, B]): OpenApi.Object =
        OpenApi.fromMap(abs.map { case (key, value) => (ofKey.value.encode(key).print, ofSchema.value.encode(value)) })
