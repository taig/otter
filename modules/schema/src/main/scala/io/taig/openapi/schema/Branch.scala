package io.taig.openapi.schema

import cats.Eval
import cats.Eq
import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.OpenApi
import io.taig.openapi.schema.schemas.*
import io.taig.validation.Constraint

sealed abstract class Branch[A, B](
    val metadata: Branch.Metadata[A],
    val key: Eval[Schema.Of[A, OpenApi.Primitive]],
    val schema: Eval[Schema[B]]
):
  final def constraints: Chain[Constraint[OpenApi]] = schema.value.constraints

  object name:
    def value: C = metadata.name
    def set[C: Eq](name: C, key: Schema.Of[C, OpenApi.Primitive]): Branch[C, B] =
      new Branch[C, B](metadata.copy(name = name), Eval.now(key), schema):
        override def matches(name: C): Boolean = name === metadata.name
    def set(name: String): Branch[String, B] = set(name, string)

  def matches(name: A): Boolean

  final infix def orElse[C](branch: Branch[A, C]) = ???
  final infix def :+[C](branch: Branch[A, C]) = ???

  final def decode(openapi: OpenApi, discriminator: Option[Discriminator]): Validated[Violations, Option[B]] = ???

  final def encode(b: B, discriminator: Option[Discriminator]): OpenApi = discriminator match
      case Some(Discriminator.Nested(identifier, value)) =>
        OpenApi.obj(identifier -> key.value.encode(metadata.name), value -> schema.value.encode(b))
      case Some(Discriminator.Merged(identifier)) =>
        schema.value.encode(b).asObject match
          case Some(obj) if obj.contains(identifier) => OpenApi.Object.Empty
          case Some(obj) => obj.deepMerge(OpenApi.obj(identifier -> key.value.encode(metadata.name)))
          case None => OpenApi.Object.Empty
      case Some(Discriminator.Keyed) =>
        OpenApi.obj(key.value.encode(metadata.name).print -> schema.value.encode(b))
      case None => schema.value.encode(b)

object Branch:
  final case class Metadata[A](name: A)

  def apply[A: Eq, B](name: A, key: Eval[Schema.Of[A, OpenApi.Primitive]], schema: Eval[Schema[B]]): Branch[A, B] =
    new Branch[A, B](???, key, schema):
      override def matches(name: A): Boolean = name === metadata.name

      override def decode(openapi: OpenApi, discriminator: Option[Discriminator]): Validated[Violations, Option[B]] =
        ???
