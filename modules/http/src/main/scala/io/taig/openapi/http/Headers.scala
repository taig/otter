package io.taig.openapi.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.schema.{andThenValidate, Evidence, InvariantValidation, Violations}
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.openapi.syntax.*
import io.taig.screening.{Validation, Violation}
import org.typelevel.ci.CIString

sealed abstract class Headers[A](val toChain: Chain[Header[?]]):
  self =>

  final def imap[B](f: A => B)(g: B => A): Headers[B] = ivalidate(Validation.fromFunction(f))(g)
  final def gimap[B](using evidence: Evidence.Product.Aux[B, A]): Headers[B] = imap(evidence.from)(evidence.to)
  final def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Headers[C] =
    new Headers[C](toChain):
      override def decodeWithRemainders(
          headers: Chain[(CIString, OpenApi.Primitive)]
      ): Validated[Violations, (Chain[(CIString, OpenApi.Primitive)], C)] = self
        .decodeWithRemainders(headers)
        .andThen(_.traverse(andThenValidate(validation, self.render(_).asOpenApi)))
      override def encode(b: C): Chain[(CIString, OpenApi.Primitive)] = self.encode(g(b))
      override def render(b: C): Chain[String] = self.render(g(b))

  infix def zip[B](headers: Headers[B]): Headers[(A, B)] = new Headers[(A, B)](toChain ++ headers.toChain):
    override def decodeWithRemainders(
        values: Chain[(CIString, OpenApi.Primitive)]
    ): Validated[Violations, (Chain[(CIString, OpenApi.Primitive)], (A, B))] = self.decodeWithRemainders(values) match
      case Validated.Valid((values, a)) =>
        headers.decodeWithRemainders(values).map { case (headers, b) => (headers, (a, b)) }
      case Validated.Invalid(violations) =>
        headers.decodeWithRemainders(values).fold(violations |+| _, _ => violations).invalid
    override def encode(ab: (A, B)): Chain[(CIString, OpenApi.Primitive)] = self.encode(ab._1) ++ headers.encode(ab._2)
    override def render(ab: (A, B)): Chain[String] = self.render(ab._1) ++ headers.render(ab._2)

  def :*[B](header: Header[B]): Headers[(A, B)] = zip(header.toHeaders)

  final def decode(headers: Chain[(CIString, OpenApi.Primitive)]): Validated[Violations, A] =
    decodeWithRemainders(headers).map(_._2)

  def decodeWithRemainders(
      headers: Chain[(CIString, OpenApi.Primitive)]
  ): Validated[Violations, (Chain[(CIString, OpenApi.Primitive)], A)]

  def encode(a: A): Chain[(CIString, OpenApi.Primitive)]

  def render(a: A): Chain[String]

object Headers:
  val Empty: Headers[Unit] = new Headers[Unit](Chain.empty):
    override def decodeWithRemainders(
        headers: Chain[(CIString, OpenApi.Primitive)]
    ): Validated[Violations, (Chain[(CIString, OpenApi.Primitive)], Unit)] = (headers, ()).valid
    override def encode(a: Unit): Chain[(CIString, OpenApi.Primitive)] = Chain.empty
    override def render(a: Unit): Chain[String] = Chain.empty

  def one[A](header: Header[A]): Headers[A] = new Headers[A](Chain.one(header)):
    override def decodeWithRemainders(
        headers: Chain[(CIString, OpenApi.Primitive)]
    ): Validated[Violations, (Chain[(CIString, OpenApi.Primitive)], A)] =
      header.decode(headers)
    override def encode(a: A): Chain[(CIString, OpenApi.Primitive)] = header.encode(a)
    override def render(a: A): Chain[String] = header.render(a)

  given InvariantValidation.Product[Headers] with
    override def unit: Headers[Unit] = Empty
    override def product[A, B](fa: Headers[A], fb: Headers[B]): Headers[(A, B)] = fa zip fb
    override def imap[A, B](fa: Headers[A])(f: A => B)(g: B => A): Headers[B] = fa.imap(f)(g)
    override def ivalidate[A: Encoder, B, C](fa: Headers[B])(validation: Validation[A, B, B, C])(
        g: C => B
    ): Headers[C] =
      fa.ivalidate(validation)(g)
