package io.taig.otter.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.http.syntax.*
import io.taig.otter.schema.{Collection, Schema, Violations}
import org.typelevel.ci.CIString

final case class Header[A](name: CIString, schema: Schema.Value[A] | Collection[Schema.Value, A]):
  def isOptional: Boolean = schema.isOptional
  def isCollection: Boolean = schema match
    case _: Collection[?, ?] => true
    case _                   => false

  def optional: Header[Option[A]] = schema match
    case schema: Schema.Value[A]             => copy(schema = schema.optional)
    case schema: Collection[Schema.Value, ?] => copy(schema = schema.optional)

  def imap[B](f: A => B)(g: B => A): Header[B] = schema match
    case schema: Schema.Value[A]             => copy(schema = schema.imap(f)(g))
    case schema: Collection[Schema.Value, A] => copy(schema = schema.imap(f)(g))

  def toHeaders: Headers[A] = Headers(this)

  def decodeWithRemainders(remainders: Http.Headers): Validated[Violations, (Http.Headers, A)] = schema match
    case schema: Schema.Value[A] =>
      remainders.firstWithRemainders(name) match
        case Some((head, tail)) => schema.parse(head.some).tupleLeft(tail)
        case None               => schema.parse(none).tupleLeft(remainders)
    case schema: Collection[Schema.Value, A] =>
      val (head, tail) = remainders.allWithRemainders(name)
      schema.parse(head.map(_.some).some).tupleLeft(tail)
  def encode(a: A): Http.Headers = schema match
    case schema: Schema.Value[A] => Chain.fromOption(schema.print(a)).tupleLeft(name)
    case schema: Collection[Schema.Value, A] =>
      Chain.fromOption(schema.print(a)).flatMap(_.mapFilter(identity)).tupleLeft(name)
