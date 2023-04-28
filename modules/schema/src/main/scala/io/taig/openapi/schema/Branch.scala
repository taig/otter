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
    val key: Eval[Value[A]],
    val schema: Eval[Schema[B]]
):
  def matches(name: A): Boolean

  final def constraints: Chain[Constraint[OpenApi]] = schema.value.constraints

  object name:
    def value: A = metadata.name
    def set[C: Eq](name: C, key: => Value[C]): Branch[C, B] =
      new Branch[C, B](metadata.copy(name = name), Eval.later(key), schema):
        override def matches(name: C): Boolean = name === metadata.name
    def set(name: String): Branch[String, B] = set(name, string)

  final infix def orElse[C](branch: Branch[A, C]) = ???
  final infix def :+[C](branch: Branch[A, C]) = ???

  // TODO imap, ivalidate, etc.

  final def decode(openapi: OpenApi, discriminator: Option[Discriminator]): Validated[Violations, Option[B]] = ???

  final def encode(b: B, discriminator: Option[Discriminator]): OpenApi = discriminator match
    case Some(Discriminator.Nested(identifier, value)) =>
      OpenApi.obj(identifier -> key.value.encode(metadata.name), value -> schema.value.encode(b))
    case Some(Discriminator.Merged(identifier)) =>
      schema.value.encode(b).asObject match
        case Some(obj) if obj.contains(identifier) => OpenApi.Object.Empty
        case Some(obj) => obj.deepMerge(OpenApi.obj(identifier -> key.value.encode(metadata.name)))
        case None      => OpenApi.Object.Empty
    case Some(Discriminator.Keyed) =>
      OpenApi.obj(key.value.encode(metadata.name).render -> schema.value.encode(b))
    case None => schema.value.encode(b)

object Branch:
  final case class Metadata[A](name: A)

  def apply[A: Eq, B](name: A, key: Eval[Value[A]], schema: Eval[Schema[B]]): Branch[A, B] =
    new Branch[A, B](Metadata(name), key, schema):
      override def matches(name: A): Boolean = name === metadata.name
