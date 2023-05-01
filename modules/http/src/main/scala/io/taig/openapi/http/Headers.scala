package io.taig.openapi.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.openapi.OpenApi
import io.taig.openapi.schema.Void
import io.taig.openapi.schema.Violations
import org.typelevel.ci.CIString

import scala.collection.immutable.VectorMap

sealed abstract class Headers[A]:
  final def product[B](headers: Headers[B]): Headers[(A, B)] = Headers.Product(this, headers)

  final transparent inline def zip[B](headers: Headers[B]): Headers[?] = inline (this, headers) match
    case (left: Headers[Void], right)  => ???
    case (left, right: Headers[Void])  => ???
    case (left: Headers[Tuple], right) => ???
    case (left, right)                 => ???

  final transparent inline def :*[B](header: Header[B]): Headers[?] = zip(header.toHeaders)

  final def decode(headers: VectorMap[CIString, OpenApi.Primitive]): Validated[Violations, A] =
    decodeWithRemainders(headers).map(_._2)

  def decodeWithRemainders(
      headers: VectorMap[CIString, OpenApi.Primitive]
  ): Validated[Violations, (VectorMap[CIString, OpenApi.Primitive], A)]

  def encode(a: A): VectorMap[CIString, OpenApi.Primitive]

object Headers:
  case object Empty extends Headers[Void]:
    override def decodeWithRemainders(
        headers: VectorMap[CIString, OpenApi.Primitive]
    ): Validated[Violations, (VectorMap[CIString, OpenApi.Primitive], Void)] =
      (headers, Void).valid
    override def encode(a: Void): VectorMap[CIString, OpenApi.Primitive] = VectorMap.empty

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

  def apply[A](header: Header[A]): Headers[A] = Root(header)
