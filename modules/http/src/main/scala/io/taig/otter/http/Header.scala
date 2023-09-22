package io.taig.otter.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.http.syntax.*
import io.taig.otter.{Schema, StringDecoder, StringEncoder}
import io.taig.otter.validation.Violations
import org.typelevel.ci.CIString

final case class Header[A](name: CIString, schema: Schema.Value[A] | Schema.Collection[Schema.Value, A]):
  def isOptional: Boolean = schema.isOptional

  def isCollection: Boolean = schema match
    case _: Schema.Collection[?, ?] => true
    case _                          => false

  def imap[B](f: A => B)(g: B => A): Header[B] = schema match
    case schema: Schema.Value[A]                    => copy(schema = schema.imap(f)(g))
    case schema: Schema.Collection[Schema.Value, A] => copy(schema = schema.imap(f)(g))

  def optional: Header[Option[A]] = schema match
    case schema: Schema.Value[A]                    => copy(schema = schema.optional)
    case schema: Schema.Collection[Schema.Value, A] => copy(schema = schema.optional)

  def toHeaders: Headers[A] = Headers(this)

  def decodeWithRemainders(remainders: Http.Headers): Validated[Violations, (Http.Headers, A)] = schema match
    case schema: Schema.Value[A] =>
      remainders.firstWithRemainders(name) match
        case Some((head, tail)) => StringDecoder.value.decode(schema, head.some).tupleLeft(tail)
        case None               => StringDecoder.value.decode(schema, none).tupleLeft(remainders)
    case schema: Schema.Collection[Schema.Value, A] =>
      val (head, tail) = remainders.allWithRemainders(name)
      StringDecoder.collection.decode(schema, head.some).tupleLeft(tail)

  def encode(a: A): Http.Headers = schema match
    case schema: Schema.Value[A] => Chain.fromOption(StringEncoder.value.encode(schema, a).tupleLeft(name))
    case schema: Schema.Collection[Schema.Value, A] =>
      StringEncoder.collection.encode(schema, a).getOrElse(Chain.empty).tupleLeft(name)
