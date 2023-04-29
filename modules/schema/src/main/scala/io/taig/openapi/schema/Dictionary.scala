package io.taig.openapi.schema

import cats.Eval
import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.OpenApi
import io.taig.validation.{Constraint, Validation}

sealed abstract class Dictionary[A](
    val constraints: Chain[Constraint[OpenApi]],
    val description: Option[String],
    val example: Option[A],
    val key: Eval[Value[?]],
    val schema: Eval[Schema[?]]
) extends Schema[A]:
  self =>

  final override type Self[a] = Dictionary[a]
  final override type Codec = OpenApi.Object

  final override def modifyDescription(f: Option[String] => Option[String]): Dictionary[A] =
    new Dictionary[A](constraints, f(description), example, key, schema) { export self.{decode, encode} }

  final override def modifyExample(f: Option[A] => Option[A]): Dictionary[A] =
    new Dictionary[A](constraints, description, f(example), key, schema) { export self.{decode, encode} }

  final override def ivalidate[B](validation: Validation[A, A, A, B])(g: B => A): Dictionary[B] =
    new Dictionary[B](
      constraints ++ validation.constraints.map(_.map(self.encode)),
      description,
      example.flatMap(validation.run(_).toOption),
      key,
      schema
    ):
      override def decode(openapi: OpenApi.Object): Validated[Violations, B] =
        self.decode(openapi).andThen(andThenValidate(validation, self.encode))
      override def encode(b: B): OpenApi.Object = self.encode(g(b))

  override def decode(openapi: OpenApi): Validated[Violations, A] = openapi match
    case openapi: OpenApi.Object => decode(openapi)
    case _                       => typeViolations("Object", openapi).invalid

  def decode(openapi: OpenApi.Object): Validated[Violations, A]

object Dictionary:
  def apply[A, B](ofKey: Eval[Value[A]], ofSchema: Eval[Schema[B]]): Dictionary[Map[A, B]] =
    new Dictionary[Map[A, B]](Chain.empty, none, none, ofKey, ofSchema):
      override def decode(openapi: OpenApi.Object): Validated[Violations, Map[A, B]] = openapi.toMap.toSeq
        .traverse { case (key, value) =>
          (ofKey.value.parse(key), ofSchema.value.decode(value)).tupled.leftMap(_.modifyHistory(key /: _))
        }
        .map(_.toMap)

      override def encode(abs: Map[A, B]): OpenApi.Object =
        OpenApi.Object(abs.map { case (key, value) => (ofKey.value.encode(key).render, ofSchema.value.encode(value)) })
