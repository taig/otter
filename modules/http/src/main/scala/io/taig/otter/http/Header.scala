package io.taig.otter.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.Collection.Of
import io.taig.otter.syntax.*
import io.taig.otter.validation.{Validation, Violations}
import io.taig.otter.{Collection, Value}
import org.typelevel.ci.CIString

sealed abstract class Header[A](val name: CIString):
  def schema: Value[?] | Collection.Of[Value[?], ?]

  final def isOptional: Boolean = schema.isOptional

  final def isCollection: Boolean = schema match
    case _: Collection[?] => true
    case _                => false

  def ivalidate[B](validation: Validation[A, B])(g: B => A): Header[B]
  final def imap[B](f: A => B)(g: B => A): Header[B] = ivalidate(Validation.lift(f))(g)
  def optional: Header[Option[A]]

  def toHeaders: Headers[A] = Headers(this)

  def decodeWithRemainders(headers: Http.Headers): Validated[Violations, (Http.Headers, A)]
  def encode(a: A): Http.Headers

object Header:
  final private class Single[A](name: CIString, val schema: Value[A]) extends Header[A](name):
    override def ivalidate[B](validation: Validation[A, B])(g: B => A): Header[B] =
      new Single[B](name, schema.ivalidate(validation)(g))
    override def optional: Header[Option[A]] = new Single[Option[A]](name, schema.optional)
    override def decodeWithRemainders(headers: Http.Headers): Validated[Violations, (Http.Headers, A)] =
      headers.firstWithRemainders(name) match
        case Some((head, tail)) => schema.parse(head.some).tupleLeft(tail)
        case None               => schema.parse(None).tupleLeft(headers)
    override def encode(a: A): Http.Headers = Chain.fromOption(schema.print(a)).tupleLeft(name)

  final private class Multiple[A](name: CIString, val schema: Collection.Of[Value[?], A]) extends Header[A](name) {
    override def ivalidate[B](validation: Validation[A, B])(g: B => A): Header[B] =
      new Multiple[B](name, schema.ivalidate(validation)(g))
    override def optional: Header[Option[A]] = new Multiple[Option[A]](name, schema.optional)
    override def decodeWithRemainders(headers: Http.Headers): Validated[Violations, (Http.Headers, A)] =
      val (head, tail) = headers.allWithRemainders(name)
      schema.parse(head.map(_.some).some).tupleLeft(tail)
    override def encode(a: A): Http.Headers =
      schema.print(a).getOrElse(Chain.empty).mapFilter(identity).tupleLeft(name)
  }

  def apply[A](name: CIString, schema: Value[A]): Header[A] = new Single[A](name, schema)
  def apply[A](name: CIString, schema: Collection.Of[Value[?], A]): Header[A] = new Multiple[A](name, schema)
