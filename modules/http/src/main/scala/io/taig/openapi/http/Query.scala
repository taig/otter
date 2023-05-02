package io.taig.openapi.http

import cats.Eval
import cats.syntax.all.*
import cats.data.Validated
import io.taig.openapi.OpenApi
import io.taig.openapi.schema.{Value, Violations}
import io.taig.openapi.validation.Constraint

import scala.collection.immutable.VectorMap

sealed abstract class Query[A]:
  def name: String

  def schema: Eval[Value[?]]

  final def optional: Query[Option[A]] = Query.Optional(this)

  def decode(
      queries: VectorMap[String, OpenApi.Primitive]
  ): Validated[Violations, (VectorMap[String, OpenApi.Primitive], A)]

  def encode(a: A): VectorMap[String, OpenApi.Primitive]

object Query:
  final private case class Root[A](name: String, schema: Eval[Value[A]]) extends Query[A]:
    override def decode(
        queries: VectorMap[String, OpenApi.Primitive]
    ): Validated[Violations, (VectorMap[String, OpenApi.Primitive], A)] = queries.get(name) match
      case Some(openapi) => schema.value.decode(openapi).tupleLeft(queries.removed(name))
      case None          => Violations.rootNec(Constraint.required.toViolation(OpenApi.fromSeqMap(queries))).invalid
    override def encode(a: A): VectorMap[String, OpenApi.Primitive] = VectorMap(name -> schema.value.encode(a))

  final private case class Optional[A](query: Query[A]) extends Query[Option[A]]:
    export query.{name, schema}

    override def decode(
        queries: VectorMap[String, OpenApi.Primitive]
    ): Validated[Violations, (VectorMap[String, OpenApi.Primitive], Option[A])] =
      if queries.contains(name) then query.decode(queries).map(_.map(_.some)) else (queries, none[A]).valid

    override def encode(a: Option[A]): VectorMap[String, OpenApi.Primitive] = a.fold(VectorMap.empty)(query.encode)
