package io.taig.openapi.http

import cats.Eval
import cats.syntax.all.*
import cats.data.Validated
import io.taig.openapi.OpenApi
import io.taig.openapi.schema.{Schema, Violations}
import io.taig.openapi.validation.Constraint

import scala.collection.immutable.VectorMap

sealed abstract class Query[A]:
  def isOptional: Boolean
  def name: String
  def schema: Eval[Schema.Value[?]]
  final def optional: Query[Option[A]] = Query.Optional(this)
  final transparent inline def &[B](query: Query[B]): Queries[?] = toQueries & query
  final def toQueries: Queries[A] = Queries(this)
  def decode(
      queries: VectorMap[String, String]
  ): Validated[Violations, (VectorMap[String, String], A)]
  def encode(a: A): VectorMap[String, String]

object Query:
  final private case class Root[A](name: String, schema: Eval[Schema.Value[A]]) extends Query[A]:
    override def isOptional: Boolean = false
    override def decode(queries: VectorMap[String, String]): Validated[Violations, (VectorMap[String, String], A)] =
      queries
        .get(name)
        .match {
          case Some(value) => schema.value.parse(value).tupleLeft(queries.removed(name))
          case None        => Violations.rootNec(Constraint.required.toViolation(OpenApi.Null)).invalid
        }
        .leftMap(_.modifyHistory(name /: _))
    override def encode(a: A): VectorMap[String, String] = VectorMap(name -> schema.value.render(a))

  final private case class Optional[A](query: Query[A]) extends Query[Option[A]]:
    export query.{name, schema}
    override def isOptional: Boolean = true
    override def decode(
        queries: VectorMap[String, String]
    ): Validated[Violations, (VectorMap[String, String], Option[A])] =
      if queries.contains(name) then query.decode(queries).map(_.map(_.some)) else (queries, none[A]).valid
    override def encode(a: Option[A]): VectorMap[String, String] = a.fold(VectorMap.empty)(query.encode)

  def apply[A](name: String, schema: Eval[Schema.Value[A]]): Query[A] = Root(name, schema)
