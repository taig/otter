package io.taig.otter.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.History
import io.taig.otter.http.Http.Payload
import io.taig.otter.validation.{Violation, Violations}

final case class Response[A](results: Results[A], violations: Result[Violations]):
  def encode(a: Validated[Violations, A]): Http.Response = a.fold(violations.encode, results.encode)

object Response:
  sealed abstract class Body[A]:
    self =>
    type Self[a] <: Body[a] { type Self[a] = self.Self[a] }

    def andThen[B](f: A => Validated[Violations, B])(g: B => A): Self[B]
    final def imap[B](f: A => B)(g: B => A): Self[B] = andThen(f(_).valid)(g)
    def zip[B](headers: Headers[B]): Self[(A, B)]

    final def decode(headers: Http.Headers, payload: Http.Payload): Validated[Violations, A] =
      decodeWithRemainders(headers, payload).map(_._2)
    def decodeWithRemainders(remainders: Http.Headers, payload: Http.Payload): Validated[Violations, (Http.Headers, A)]
    def encode(a: A): (Http.Headers, Http.Payload)

  object Body extends ToResponseBodyOps:
    sealed abstract class Strict[A] extends Response.Body[A]:
      self =>
      final override type Self[a] = Response.Body.Strict[a]

      final override def andThen[B](f: A => Validated[Violations, B])(g: B => A): Response.Body.Strict[B] =
        new Strict[B]:
          override def decodeWithRemainders(
              remainders: Http.Headers,
              payload: Array[Byte]
          ): Validated[Violations, (Http.Headers, B)] =
            self.decodeWithRemainders(remainders, payload).andThen(_.traverse(f))
          override def encode(b: B): (Http.Headers, Payload.Strict) = self.encode(g(b))

      final override def zip[B](headers: Headers[B]): Response.Body.Strict[(A, B)] = new Strict[(A, B)]:
        override def decodeWithRemainders(
            remainders: Http.Headers,
            payload: Array[Byte]
        ): Validated[Violations, (Http.Headers, (A, B))] =
          self.decodeWithRemainders(remainders, payload).andThen { case (remainders, a) =>
            headers.decodeWithRemainders(remainders).map(_.tupleLeft(a))
          }
        override def encode(ab: (A, B)): (Http.Headers, Payload.Strict) =
          self.encode(ab._1).leftMap(_ ++ headers.encode(ab._2))

      final override def decodeWithRemainders(
          remainders: Http.Headers,
          payload: Http.Payload
      ): Validated[Violations, (Http.Headers, A)] = payload match
        case Http.Payload.Strict(data) => decodeWithRemainders(remainders, data)
        case Http.Payload.Streaming(_) =>
          Violations.oneNec(History.Root / "body", Violation.tpe("strict", "streaming")).invalid
      def decodeWithRemainders(remainders: Http.Headers, payload: Array[Byte]): Validated[Violations, (Http.Headers, A)]
      override def encode(a: A): (Http.Headers, Http.Payload.Strict)

    object Strict:
      val Bytes: Response.Body.Strict[Array[Byte]] = new Strict[Array[Byte]]:
        override def decodeWithRemainders(
            remainders: Http.Headers,
            payload: Array[Byte]
        ): Validated[Violations, (Http.Headers, Array[Byte])] = (remainders, payload).valid
        override def encode(data: Array[Byte]): (Http.Headers, Http.Payload.Strict) =
          (Chain.empty, Http.Payload.Strict(data))
