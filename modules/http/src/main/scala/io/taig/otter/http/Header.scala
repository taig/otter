package io.taig.otter.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.syntax.*
import io.taig.otter.validation.{Validation, Violations}
import io.taig.otter.{Collection, Schema}
import org.typelevel.ci.CIString

final case class Header[A](name: CIString, schema: Schema.Value[A] | Collection.Value[A]):
  def isOptional: Boolean = schema.isOptional

  def isCollection: Boolean = schema match
    case _: Collection[?] => true
    case _                => false

  def imap[B](f: A => B)(g: B => A): Header[B] = schema match
    case schema: Schema.Value[A]     => copy(schema = schema.imap(f)(g))
    case schema: Collection.Value[A] => copy(schema = schema.imap(f)(g))

  def ivalidate[B](validation: Validation[A, B])(g: B => A): Header[B] = schema match
    case schema: Schema.Value[A]     => copy(schema = schema.ivalidate(validation)(g))
    case schema: Collection.Value[A] => copy(schema = schema.ivalidate(validation)(g))

  def optional: Header[Option[A]] = schema match
    case schema: Schema.Value[A]     => copy(schema = schema.optional)
    case schema: Collection.Value[A] => copy(schema = schema.optional)

  def toHeaders: Headers[A] = Headers(this)

  def decodeWithRemainders(remainders: Http.Headers): Validated[Violations, (Http.Headers, A)] = schema match
    case schema: Schema.Value[A] =>
      remainders.firstWithRemainders(name) match
        case Some((head, tail)) => schema.parse(head.some).tupleLeft(tail)
        case None               => schema.parse(None).tupleLeft(remainders)
    case schema: Collection.Value[A] =>
      val (head, tail) = remainders.allWithRemainders(name)
      schema.parse(head.map(_.some).some).tupleLeft(tail)

  def encode(a: A): Http.Headers = schema match
    case schema: Schema.Value[A]     => Chain.fromOption(schema.print(a).tupleLeft(name))
    case schema: Collection.Value[A] => schema.print(a).getOrElse(Chain.empty).mapFilter(identity).tupleLeft(name)
