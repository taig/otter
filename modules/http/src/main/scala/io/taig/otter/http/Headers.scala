package io.taig.otter.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.schema.Violations

sealed abstract class Headers[A]:
  self =>
  def toChain: Chain[Header[?]]

  final def imap[B](f: A => B)(g: B => A): Headers[B] = new Headers[B]:
    export self.toChain
    override def decodeWithRemainders(remainders: Http.Headers): Validated[Violations, (Http.Headers, B)] =
      self.decodeWithRemainders(remainders).map(_.map(f))
    override def encode(b: B): Http.Headers = self.encode(g(b))

  final def zip[B](headers: Headers[B]): Headers[(A, B)] = new Headers[(A, B)]:
    export self.toChain
    override def decodeWithRemainders(remainders: Http.Headers): Validated[Violations, (Http.Headers, (A, B))] =
      self.decodeWithRemainders(remainders) match
        case Validated.Valid((remainders, a)) => headers.decodeWithRemainders(remainders).map(_.tupleLeft(a))
        case Validated.Invalid(left) =>
          headers.decodeWithRemainders(remainders) match
            case Validated.Valid(_)       => left.invalid
            case Validated.Invalid(right) => (left |+| right).invalid
    override def encode(ab: (A, B)): Http.Headers = self.encode(ab._1) ++ headers.encode(ab._2)

  final def decode(headers: Http.Headers): Validated[Violations, A] = decodeWithRemainders(headers).map(_._2)
  def decodeWithRemainders(remainders: Http.Headers): Validated[Violations, (Http.Headers, A)]
  def encode(a: A): Http.Headers

object Headers:
  val Empty: Headers[Unit] = new Headers[Unit]:
    override def toChain: Chain[Header[?]] = Chain.empty
    override def decodeWithRemainders(remainders: Http.Headers): Validated[Violations, (Http.Headers, Unit)] =
      (remainders, ()).valid
    override def encode(a: Unit): Http.Headers = Chain.empty

  def apply[A](header: Header[A]): Headers[A] = new Headers[A]:
    override def toChain: Chain[Header[A]] = Chain.one(header)
    override def decodeWithRemainders(remainders: Http.Headers): Validated[Violations, (Http.Headers, A)] =
      header.decodeWithRemainders(remainders)
    override def encode(a: A): Http.Headers = header.encode(a)
