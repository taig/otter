package io.taig.openapi.http

import cats.{Invariant, InvariantSemigroupal}
import cats.data.Validated
import cats.syntax.all.*
import io.taig.openapi.schema.{Violations, Void}
import org.typelevel.ci.CIString

import scala.collection.immutable.VectorMap

sealed abstract class Headers[A]:
  def matches(headers: VectorMap[CIString, String]): Boolean
  final def product[B](headers: Headers[B]): Headers[(A, B)] = Headers.Product(this, headers)
  final transparent inline def zip[B](headers: Headers[B]): Headers[?] = inline (this, headers) match
    case (left: Headers[Void], right) => left.product(right).imap[B] { case (_, b) => b }(b => (Void, b))
    case (left, right: Headers[Void]) => left.product(right).imap[A] { case (a, _) => a }(a => (a, Void))
    case (left: Headers[? *: ?], right) =>
      left.product(right).imap { case (a, b) => a :* b }(ab => (ab.init.asInstanceOf[A], ab.last.asInstanceOf[B]))
    case (left, right) => left.product(right)
  final transparent inline def :*[B](header: Header[B]): Headers[?] = zip(header.toHeaders)
  final def imap[B](f: A => B)(g: B => A): Headers[B] = Headers.Modify(this, f, g)
  final def decode(headers: VectorMap[CIString, String]): Validated[Violations, A] =
    decodeWithRemainders(headers).map(_._2)
  def decodeWithRemainders(
      headers: VectorMap[CIString, String]
  ): Validated[Violations, (VectorMap[CIString, String], A)]
  def encode(a: A): VectorMap[CIString, String]

object Headers:
  final private case class Root[A](header: Header[A]) extends Headers[A]:
    override def matches(headers: VectorMap[CIString, String]): Boolean = ???
    override def decodeWithRemainders(
        headers: VectorMap[CIString, String]
    ): Validated[Violations, (VectorMap[CIString, String], A)] = header.decode(headers)
    override def encode(a: A): VectorMap[CIString, String] = header.encode(a)

  final private case class Product[A, B](left: Headers[A], right: Headers[B]) extends Headers[(A, B)]:
    override def matches(headers: VectorMap[CIString, String]): Boolean = ???
    override def decodeWithRemainders(
        headers: VectorMap[CIString, String]
    ): Validated[Violations, (VectorMap[CIString, String], (A, B))] = left.decodeWithRemainders(headers) match
      case Validated.Valid((remainders, a)) => right.decodeWithRemainders(remainders).map(_.tupleLeft(a))
      case Validated.Invalid(violations)    => right.decode(headers).fold(violations merge _, _ => violations).invalid
    override def encode(ab: (A, B)): VectorMap[CIString, String] = left.encode(ab._1) ++ right.encode(ab._2)

  final private case class Modify[A, B](
      headers: Headers[A],
      f: A => B,
      g: B => A
  ) extends Headers[B]:
    export headers.matches
    override def decodeWithRemainders(
        values: VectorMap[CIString, String]
    ): Validated[Violations, (VectorMap[CIString, String], B)] = headers.decodeWithRemainders(values).map(_.map(f))
    override def encode(b: B): VectorMap[CIString, String] = headers.encode(g(b))

  val Empty: Headers[Void] = new Headers[Void]:
    override def matches(headers: VectorMap[CIString, String]): Boolean = true
    override def decodeWithRemainders(
        headers: VectorMap[CIString, String]
    ): Validated[Violations, (VectorMap[CIString, String], Void)] = (headers, Void).valid
    override def encode(a: Void): VectorMap[CIString, String] = VectorMap.empty

  def apply[A](header: Header[A]): Headers[A] = Root(header)

  given InvariantSemigroupal[Headers] with
    override def imap[A, B](fa: Headers[A])(f: A => B)(g: B => A): Headers[B] = fa.imap(f)(g)
    override def product[A, B](fa: Headers[A], fb: Headers[B]): Headers[(A, B)] = fa.product(fb)
