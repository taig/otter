package io.taig.openapi.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import cats.{Eval, Invariant}
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.openapi.schema.{Evidence, InvariantValidation, Schema, Violations}
import io.taig.screening.{Validation, Violation}
import org.typelevel.ci.CIString

final case class Header[A](name: CIString, schema: Eval[Header.Value[A]]):
  def modifyName(f: CIString => CIString): Header[A] = copy(name = f(name))
  def modifySchema[B](f: Header.Value[A] => Header.Value[B]): Header[B] = copy(schema = schema.map(f))

  def imap[B](f: A => B)(g: B => A): Header[B] = modifySchema(_.imap(f)(g))
  def gimap[B](using evidence: Evidence.Product.Aux[B, A]): Header[B] = imap(evidence.from)(evidence.to)
  def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Header[C] =
    modifySchema(_.ivalidate(validation)(g))

  def optional: Header[Option[A]] = copy(
    name = name,
    schema = schema.map {
      case Header.Value.Required(schema) => Header.Value.Optional(schema.optional)
      case Header.Value.Optional(schema) => Header.Value.Optional(schema.optional)
    }
  )

  infix def zip[B](header: Header[B]): Headers[(A, B)] = toHeaders zip header.toHeaders
  def :*[B](header: Header[B]): Headers[(A, B)] = zip(header)

  def toHeaders: Headers[A] = Headers.one(this)

  def decode(
      values: Chain[(CIString, OpenApi.Primitive)]
  ): Validated[Violations, (Chain[(CIString, OpenApi.Primitive)], A)] =
    val (remainders, value) = collectAndRemoveFirst(values) { case (`name`, value) => value }
    val openapi = value.getOrElse(OpenApi.Null)
    schema.value.decode(openapi).tupleLeft(remainders).leftMap(_.modifyHistory(name.toString /: _))

  def encode(a: A): Chain[(CIString, OpenApi.Primitive)] = schema.value.encode(a) match
    case OpenApi.Null               => Chain.empty
    case openapi: OpenApi.Primitive => Chain.one(name -> openapi)

  def render(a: A): Chain[String] = encode(a).map { case (name, value) => s"$name: ${value.print}" }

object Header:
  enum Value[A]:
    case Required(schema: Schema.Of[A, OpenApi.Primitive])
    case Optional(schema: Schema.Of[A, OpenApi.Null.type | OpenApi.Primitive])

    final def imap[B](f: A => B)(g: B => A): Header.Value[B] = this match
      case Required(schema) => Required(schema.imap(f)(g))
      case Optional(schema) => Optional(schema.imap(f)(g))

    final def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Header.Value[C] = this match
      case Required(schema) => Required(schema.ivalidate(validation)(g))
      case Optional(schema) => Optional(schema.ivalidate(validation)(g))

    final def decode(openapi: OpenApi): Validated[Violations, A] = this match
      case Required(schema) => schema.decode(openapi)
      case Optional(schema) => schema.decode(openapi)

    final def encode(a: A): OpenApi.Null.type | OpenApi.Primitive = this match
      case Required(schema) => schema.encode(a)
      case Optional(schema) => schema.encode(a)

  given InvariantValidation[Header] with
    override def imap[A, B](fa: Header[A])(f: A => B)(g: B => A): Header[B] = fa.imap(f)(g)
    override def ivalidate[A: Encoder, B, C](fa: Header[B])(validation: Validation[A, B, B, C])(g: C => B): Header[C] =
      fa.ivalidate(validation)(g)
