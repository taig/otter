package io.taig.otter.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.schema.{Collection, Schema, Violations}
import io.taig.otter.http.syntax.*
import org.typelevel.ci.CIString

sealed abstract class Header[A]:
  self =>
  def name: CIString
  def schema: Schema.Value[?] | Collection.Value[?]
  final def isOptional: Boolean = schema.isOptional
  final def isCollection: Boolean = schema match
    case _: Collection[?] => true
    case _                => false

  final def imap[B](f: A => B)(g: B => A): Header[B] = new Header[B]:
    export self.{name, schema}
    override def decodeWithRemainders(remainders: Http.Headers): Validated[Violations, (Http.Headers, B)] =
      self.decodeWithRemainders(remainders).map(_.map(f))

    override def encode(b: B): Http.Headers = self.encode(g(b))

  final def toHeaders: Headers[A] = Headers(this)

  def decodeWithRemainders(remainders: Http.Headers): Validated[Violations, (Http.Headers, A)]
  def encode(a: A): Http.Headers

object Header:
  def apply[A](value: CIString, of: Schema.Value[A]): Header[A] = new Header[A]:
    override def name: CIString = value
    override def schema: Schema.Value[A] = of
    override def decodeWithRemainders(remainders: Http.Headers): Validated[Violations, (Http.Headers, A)] =
      remainders.firstWithRemainders(name) match
        case Some((head, tail)) => schema.parse(head.some).tupleLeft(tail)
        case None               => schema.parse(none).tupleLeft(remainders)
    override def encode(a: A): Http.Headers = Chain.fromOption(of.print(a)).tupleLeft(name)

  def apply[A](collection: CIString, of: Collection.Value[A]): Header[A] = new Header[A]:
    override def name: CIString = collection
    override def schema: Collection.Value[A] = of
    override def decodeWithRemainders(remainders: Http.Headers): Validated[Violations, (Http.Headers, A)] =
      val (head, tail) = remainders.allWithRemainders(name)
      schema.parse(head.map(_.some).some).tupleLeft(tail)
    override def encode(a: A): Http.Headers =
      Chain.fromOption(of.print(a)).flatMap(_.mapFilter(identity)).tupleLeft(name)
