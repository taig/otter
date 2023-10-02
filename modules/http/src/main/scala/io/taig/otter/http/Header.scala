package io.taig.otter.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.Collection.Of
import io.taig.otter.syntax.*
import io.taig.otter.validation.{Validation, Violations}
import io.taig.otter.{Collection, Value}
import org.typelevel.ci.CIString

sealed abstract class Header[A](val name: CIString):
  def codec: Value[?] | Collection.Of[Value[?], ?]

  final def isOptional: Boolean = codec.isOptional

  final def isCollection: Boolean = codec match
    case _: Collection[?] => true
    case _                => false

  def ivalidate[B](validation: Validation[A, B])(g: B => A): Header[B]
  final def imap[B](f: A => B)(g: B => A): Header[B] = ivalidate(Validation.lift(f))(g)
  def optional: Header[Option[A]]

  def toHeaders: Headers[A] = Headers(this)

  def decodeWithRemainders(headers: Http.Headers): Validated[Violations, (Http.Headers, A)]
  def encode(a: A): Http.Headers

object Header:
  final private class Single[A](name: CIString, val codec: Value[A]) extends Header[A](name):
    override def ivalidate[B](validation: Validation[A, B])(g: B => A): Header[B] =
      new Single[B](name, codec.ivalidate(validation)(g))
    override def optional: Header[Option[A]] = new Single[Option[A]](name, codec.optional)
    override def decodeWithRemainders(headers: Http.Headers): Validated[Violations, (Http.Headers, A)] =
      headers.firstWithRemainders(name) match
        case Some((head, tail)) => codec.parse(head.some).tupleLeft(tail)
        case None               => codec.parse(None).tupleLeft(headers)
    override def encode(a: A): Http.Headers = codec.print(a) match
      case value: String         => Chain.one(name -> value)
      case value: Option[String] => Chain.fromOption(value).tupleLeft(name)

  final private class Multiple[A](name: CIString, val codec: Collection.Of[Value[?], A]) extends Header[A](name):
    override def ivalidate[B](validation: Validation[A, B])(g: B => A): Header[B] =
      new Multiple[B](name, codec.ivalidate(validation)(g))
    override def optional: Header[Option[A]] = new Multiple[Option[A]](name, codec.optional)
    override def decodeWithRemainders(headers: Http.Headers): Validated[Violations, (Http.Headers, A)] =
      val (head, tail) = headers.allWithRemainders(name)
      codec.parse(head.some).tupleLeft(tail)
    override def encode(a: A): Http.Headers = codec.print(a).getOrElse(Chain.empty).tupleLeft(name)

  def apply[A](name: CIString, codec: Value[A]): Header[A] = new Single[A](name, codec)
  def apply[A](name: CIString, codec: Collection.Of[Value[?], A]): Header[A] = new Multiple[A](name, codec)
