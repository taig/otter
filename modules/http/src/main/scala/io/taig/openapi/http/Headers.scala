package io.taig.openapi.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.openapi.schema.Void
import io.taig.openapi.schema.applyValidation
import io.taig.openapi.schema.Violations
import io.taig.openapi.validation.Validation
import org.typelevel.ci.CIString

import scala.collection.immutable.VectorMap

sealed abstract class Headers[A]:
  final def product[B](headers: Headers[B]): Headers[(A, B)] = Headers.Product(this, headers)

  final transparent inline def zip[B](headers: Headers[B]): Headers[?] = inline (this, headers) match
    case (left: Headers[Void], right) => left.product(right).imap[B] { case (_, b) => b }(b => (Void, b))
    case (left, right: Headers[Void]) => left.product(right).imap[A] { case (a, _) => a }(a => (a, Void))
    case (left: Headers[Tuple], right) =>
      left.product(right).imap[Tuple.Append[A, B]] { case (a, b) => a :* b }(ab => (ab.init, ab.last.asInstanceOf[B]))
    case (left, right) => left.product(right)

  final transparent inline def :*[B](header: Header[B]): Headers[?] = zip(header.toHeaders)

  final def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Headers[C] =
    Headers.Validate(this, validation, g)

  final def imap[B](f: A => B)(g: B => A): Headers[B] = ivalidate(Validation.lift(f))(g)

  final def decode(headers: VectorMap[CIString, OpenApi.Primitive]): Validated[Violations, A] =
    decodeWithRemainders(headers).map(_._2)

  def decodeWithRemainders(
      headers: VectorMap[CIString, OpenApi.Primitive]
  ): Validated[Violations, (VectorMap[CIString, OpenApi.Primitive], A)]

  def encode(a: A): VectorMap[CIString, OpenApi.Primitive]

object Headers:
  final private case class Root[A](header: Header[A]) extends Headers[A]:
    override def decodeWithRemainders(
        headers: VectorMap[CIString, OpenApi.Primitive]
    ): Validated[Violations, (VectorMap[CIString, OpenApi.Primitive], A)] = header.decode(headers)
    override def encode(a: A): VectorMap[CIString, OpenApi.Primitive] = header.encode(a)

  final private case class Product[A, B](left: Headers[A], right: Headers[B]) extends Headers[(A, B)]:
    override def decodeWithRemainders(
        headers: VectorMap[CIString, OpenApi.Primitive]
    ): Validated[Violations, (VectorMap[CIString, OpenApi.Primitive], (A, B))] =
      left.decodeWithRemainders(headers) match
        case Validated.Valid((remainders, a)) => right.decodeWithRemainders(remainders).map(_.tupleLeft(a))
        case Validated.Invalid(violations)    => right.decode(headers).fold(violations merge _, _ => violations).invalid
    override def encode(ab: (A, B)): VectorMap[CIString, OpenApi.Primitive] = left.encode(ab._1) ++ right.encode(ab._2)

  final private case class Validate[A, B: Encoder, C](
      headers: Headers[A],
      validation: Validation[B, A, A, C],
      g: C => A
  ) extends Headers[C]:
    override def decodeWithRemainders(
        values: VectorMap[CIString, OpenApi.Primitive]
    ): Validated[Violations, (VectorMap[CIString, OpenApi.Primitive], C)] = headers
      .decodeWithRemainders(values)
      .andThen(
        _.traverse(
          applyValidation(
            validation,
            a => OpenApi.fromMap(headers.encode(a).map { case (key, value) => (key.toString, value) })
          )
        )
      )
    override def encode(c: C): VectorMap[CIString, OpenApi.Primitive] = headers.encode(g(c))

  val Empty: Headers[Void] = new Headers[Void]:
    override def decodeWithRemainders(
        headers: VectorMap[CIString, OpenApi.Primitive]
    ): Validated[Violations, (VectorMap[CIString, OpenApi.Primitive], Void)] = (headers, Void).valid
    override def encode(a: Void): VectorMap[CIString, OpenApi.Primitive] = VectorMap.empty

  def apply[A](header: Header[A]): Headers[A] = Root(header)
