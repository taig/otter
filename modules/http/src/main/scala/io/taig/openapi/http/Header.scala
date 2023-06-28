package io.taig.openapi.http

import cats.Eval
import cats.syntax.all.*
import cats.data.{Chain, NonEmptyChain, Validated}
import io.taig.openapi.OpenApi
import io.taig.openapi.schema.{Collection, Schema, Violations}
import io.taig.openapi.validation.Constraint
import org.typelevel.ci.CIString

import scala.collection.immutable.VectorMap

// TODO default (?)
sealed abstract class Header[A]:
  def name: CIString
  def schema: Eval[Schema[?]]
  final def optional: Header[Option[A]] = Header.Optional(this)
  final def toHeaders: Headers[A] = Headers(this)
  def decode(headers: Http.Headers): Validated[Violations, (Http.Headers, A)]
  def encode(a: A): Http.Headers

object Header:
  final private case class Single[A](name: CIString, schema: Eval[Schema.Value[A]]) extends Header[A]:
    override def decode(headers: Http.Headers): Validated[Violations, (Http.Headers, A)] =
      headers.getFirstWithRemainders(name) match
        case Some((head, remainders)) => schema.value.parse(head).tupleLeft(remainders)
        case None                     => Violations.rootNec(Constraint.required.toViolation(OpenApi.Null)).invalid
    override def encode(a: A): Http.Headers = Http.Headers.one(name, schema.value.render(a))

  final private case class Multiple[A](name: CIString, schema: Eval[Collection.Value[A]]) extends Header[A]:
    override def decode(headers: Http.Headers): Validated[Violations, (Http.Headers, A)] =
      headers.getWithRemainders(name) match
        case Some((headers, remainders)) => schema.value.parse(headers.toChain).tupleLeft(remainders)
        case None                        => schema.value.parse(Chain.empty).tupleLeft(headers)

    override def encode(a: A): Http.Headers =
      val values: Chain[String] = schema.value.encode(a).toChain.map(_.render)
      Http.Headers(values.tupleLeft(name))

  final private case class Optional[A](header: Header[A]) extends Header[Option[A]]:
    export header.{name, schema}
    override def decode(values: Http.Headers): Validated[Violations, (Http.Headers, Option[A])] =
      values.getFirst(name) match
        case Some(_) => header.decode(values).map(_.map(_.some))
        case None    => (values, none[A]).valid
    override def encode(a: Option[A]): Http.Headers = a.fold(Http.Headers.Empty)(header.encode)

  def single[A](name: CIString, schema: Eval[Schema.Value[A]]): Header[A] = Single(name, schema)
  def multiple[A](name: CIString, schema: Eval[Collection.Value[A]]): Header[A] = Multiple(name, schema)
