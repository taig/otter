package io.taig.openapi.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import cats.{Eval, Invariant}
import io.taig.openapi.schema.{Evidence, InvariantValidation, Schema, Violations}
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.validation.Validation

final case class Query[A](name: String, schema: Eval[Query.Value[A]]):
  def modifyName(f: String => String): Query[A] = copy(name = f(name))
  def modifySchema[B](f: Query.Value[A] => Query.Value[B]): Query[B] = copy(schema = schema.map(f))

  def imap[B](f: A => B)(g: B => A): Query[B] = modifySchema(_.imap(f)(g))
  def gimap[B](using evidence: Evidence.Product.Aux[B, A]): Query[B] = imap(evidence.from)(evidence.to)
  def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Query[C] =
    modifySchema(_.ivalidate(validation)(g))

  def zip[B](query: Query[B]): Queries[(A, B)] = toQueries :& query
  def :&[B](query: Query[B]): Queries[(A, B)] = zip(query)

  def toQueries: Queries[A] = Queries.one(this)

  def decode(
      queries: Chain[(String, OpenApi.Primitive)]
  ): Validated[Violations, (Chain[(String, OpenApi.Primitive)], A)] =
    val (remainders, value) = collectAndRemoveFirst(queries) { case (name, value) if this.name === name => value }

    schema.value
      .decode(value.getOrElse(OpenApi.Null))
      .tupleLeft(remainders)
      .leftMap(_.modifyHistory(name /: _))

  def encode(a: A): Chain[(String, OpenApi.Primitive)] = schema.value.encode(a) match
    case OpenApi.Null               => Chain.empty
    case openapi: OpenApi.Primitive => Chain.one((name, openapi))

  def render(a: A): Chain[String] = encode(a).map { case (name, value) => s"$name=${value.print}" }

object Query:
  enum Value[A]:
    case Required(schema: Schema.Of[A, OpenApi.Primitive])
    case Optional(schema: Schema.Of[A, OpenApi.Null.type | OpenApi.Primitive])

    final def imap[B](f: A => B)(g: B => A): Query.Value[B] = this match
      case Required(schema) => Required(schema.imap(f)(g))
      case Optional(schema) => Optional(schema.imap(f)(g))

    final def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Query.Value[C] = this match
      case Required(schema) => Required(schema.ivalidate(validation)(g))
      case Optional(schema) => Optional(schema.ivalidate(validation)(g))

    final def decode(openapi: OpenApi): Validated[Violations, A] = this match
      case Required(schema) => schema.decode(openapi)
      case Optional(schema) => schema.decode(openapi)

    final def encode(a: A): OpenApi.Null.type | OpenApi.Primitive = this match
      case Required(schema) => schema.encode(a)
      case Optional(schema) => schema.encode(a)

  given InvariantValidation[Query] with
    override def imap[A, B](fa: Query[A])(f: A => B)(g: B => A): Query[B] = fa.imap(f)(g)
    override def ivalidate[A: Encoder, B, C](fa: Query[B])(validation: Validation[A, B, B, C])(g: C => B): Query[C] =
      fa.ivalidate(validation)(g)
