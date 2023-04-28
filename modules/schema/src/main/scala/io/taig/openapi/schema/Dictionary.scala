package io.taig.openapi.schema

import cats.Eval
import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.OpenApi
import io.taig.validation.{Constraint, Validation}

sealed abstract class Dictionary[A](
    val constraints: Chain[Constraint[OpenApi]],
    val key: Eval[Schema.Of[?, OpenApi.Primitive]],
    val metadata: Dictionary.Metadata[A],
    val schema: Eval[Schema[?]]
) extends Schema[A]:
  self =>

  final override type Self[a] = Dictionary[a]
  final override type Codec = OpenApi.Object
  final override type Metadata[a] = Dictionary.Metadata[a]

  override def copy(metadata: Dictionary.Metadata[A]): Dictionary[A] =
    new Dictionary[A](constraints, key, metadata, schema) { export self.{decode, encode} }

  override def ivalidate[B](validation: Validation[A, A, A, B])(g: B => A): Dictionary[B] = new Dictionary[B](
    constraints ++ validation.constraints.map(_.map(self.encode)),
    key,
    metadata.flatMap(validation.run(_).toOption),
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
  final case class Metadata[A](description: Option[String], example: Option[A]) extends Schema.Metadata[A]:
    override type Self[a] = Dictionary.Metadata[a]
    override def map[B](f: A => B): Dictionary.Metadata[B] = copy(example = example.map(f))
    override def flatMap[B](f: A => Option[B]): Dictionary.Metadata[B] = copy(example = example.flatMap(f))
    override def updated(description: Option[String], example: Option[A]): Dictionary.Metadata[A] =
      Metadata(description, example)

  object Metadata:
    def empty[A]: Dictionary.Metadata[A] = Metadata(none, none)

  def apply[A, B](ofKey: Eval[Schema.Of[A, OpenApi.Primitive]], ofSchema: Eval[Schema[B]]): Dictionary[Map[A, B]] =
    new Dictionary[Map[A, B]](Chain.empty, ofKey, Metadata.empty, ofSchema):
      override def decode(openapi: OpenApi.Object): Validated[Violations, Map[A, B]] =
        openapi.toMap.toSeq
          .traverse { case (key, value) =>
            // TODO decoding the key from string will fail if the schema is a different primitive
            (ofKey.value.decode(OpenApi.fromString(key)), ofSchema.value.decode(value)).tupled
              .leftMap(_.modifyHistory(key /: _))
          }
          .map(_.toMap)

      override def encode(abs: Map[A, B]): OpenApi.Object =
        OpenApi.Object(abs.map { case (key, value) => (ofKey.value.encode(key).render, ofSchema.value.encode(value)) })
