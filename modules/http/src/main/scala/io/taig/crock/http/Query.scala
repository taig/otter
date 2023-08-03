package io.taig.crock.http

import cats.Eval
import cats.syntax.all.*
import io.taig.crock.schema.{Collection, Schema}

final case class Query[A](name: String, schema: Eval[Schema.Value[A] | Collection[A]]):
  def isOptional: Boolean = schema.value.isOptional
  def isCollection: Boolean = schema.value match
    case _: Collection[?]   => true
    case _: Schema.Value[?] => false
//  final transparent inline def &[B](query: Query[B]): Queries[?] = toQueries & query
//  final def toQueries: Queries[A] = Queries(this)
//
//object Query:
//  final private case class Single[A](name: String, schema: Eval[Schema.Value[A]]) extends Query[A]:
//    override def isOptional: Boolean = false
//    override def decode(queries: Http.Queries): Validated[Violations, (Http.Queries, A)] =
//      queries.getFirstWithRemainders(name) match
//        case Some((head, remainders)) => schema.value.parse(head).tupleLeft(remainders)
//        case None                     => Violations.rootNec(Constraint.required.toViolation(OpenApi.Null)).invalid
//    override def encode(a: A): Http.Queries = Http.Queries.one(name, schema.value.render(a))
//
//  final private case class Optional[A](query: Query[A]) extends Query[Option[A]]:
//    export query.{name, schema}
//    override def isOptional: Boolean = true
//    override def decode(queries: Http.Queries): Validated[Violations, (Http.Queries, Option[A])] =
//      queries.getFirst(name) match
//        case Some(_) => query.decode(queries).map(_.map(_.some))
//        case None    => (queries, none[A]).valid
//    override def encode(a: Option[A]): Http.Queries = a.fold(Http.Queries.Empty)(query.encode)
