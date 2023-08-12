package io.taig.otter.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.OpenApi
import io.taig.otter.http.Http.{Payload, Request}
import io.taig.otter.http.Http.Request.Body
import io.taig.otter.schema.{History, Violations}
import io.taig.otter.validation.{Constraint, Violation}

sealed abstract class Request[A]:
  self =>
  def method: Method
  def url: Url[?]
  def headers: Headers[?]
  def body: Request.Body[?]

  final def matches(method: Method, url: Http.Url): Boolean = this.method === method && this.url.matches(url)

  final def andThen[B](f: A => Validated[Violations, B])(g: B => A): Request[B] = new Request[B]:
    export self.{body, headers, method, url}
    override def decode(request: Http.Request): Validated[Violations, B] = self.decode(request).andThen(f)
    override def encode(b: B): Http.Request = self.encode(g(b))

  final def imap[B](f: A => B)(g: B => A): Request[B] = andThen(f(_).valid)(g)

  def decode(request: Http.Request): Validated[Violations, A]
  def encode(a: A): Http.Request

object Request:
  sealed abstract class Body[A]:
    self =>
    type Self[a] <: Body[a] { type Self[a] = self.Self[a] }

    def andThen[B](f: A => Validated[Violations, B])(g: B => A): Self[B]
    final def imap[B](f: A => B)(g: B => A): Self[B] = andThen(f(_).valid)(g)
    def zip[B](headers: Headers[B]): Self[(A, B)]

    def decode(headers: Http.Headers, body: Http.Request.Body): Validated[Violations, A]
    def encode(a: A): (Http.Headers, Http.Request.Body)

  object Body extends ToRequestBodyOps:
    sealed abstract class Singlepart[A] extends Request.Body[A]:
      self =>

      override type Self[a] <: Request.Body.Singlepart[a] { type Self[a] = self.Self[a] }

      override def decode(headers: Http.Headers, body: Http.Request.Body): Validated[Violations, A] = body match
        case Http.Request.Body.Singlepart(payload) => decode(headers, payload)
      def decode(headers: Http.Headers, body: Http.Payload): Validated[Violations, A]
      override def encode(a: A): (Http.Headers, Http.Request.Body.Singlepart)

    object Singlepart:
      sealed abstract class Strict[A] extends Request.Body.Singlepart[A]:
        self =>
        final override type Self[a] = Request.Body.Singlepart.Strict[a]

        final override def andThen[B](f: A => Validated[Violations, B])(g: B => A): Request.Body.Singlepart.Strict[B] =
          new Strict[B]:
            override def decodeWithRemainders(
                remainders: Http.Headers,
                payload: Array[Byte]
            ): Validated[Violations, (Http.Headers, B)] =
              self.decodeWithRemainders(remainders, payload).andThen(_.traverse(f))
            override def encode(b: B): (Http.Headers, Http.Request.Body.Singlepart) = self.encode(g(b))

        final override def zip[B](headers: Headers[B]): Request.Body.Singlepart.Strict[(A, B)] = new Strict[(A, B)]:
          override def decodeWithRemainders(
              remainders: Http.Headers,
              payload: Array[Byte]
          ): Validated[Violations, (Http.Headers, (A, B))] =
            self.decodeWithRemainders(remainders, payload).andThen { case (remainders, a) =>
              headers.decodeWithRemainders(remainders).map(_.tupleLeft(a))
            }

          override def encode(ab: (A, B)): (Http.Headers, Http.Request.Body.Singlepart) =
            self.encode(ab._1).leftMap(_ ++ headers.encode(ab._2))

        final override def decode(headers: Http.Headers, payload: Http.Payload): Validated[Violations, A] =
          payload match
            case Payload.Strict(data) => decode(headers, data)
            case Payload.Streaming(_) => Violations.rootNec(Violation.tpe("strict", "streaming")).invalid
        def decode(headers: Http.Headers, payload: Array[Byte]): Validated[Violations, A] =
          decodeWithRemainders(headers, payload).map(_._2)
        def decodeWithRemainders(
            remainders: Http.Headers,
            payload: Array[Byte]
        ): Validated[Violations, (Http.Headers, A)]

      object Strict:
        val Root: Request.Body.Singlepart.Strict[Array[Byte]] = new Strict[Array[Byte]]:
          override def encode(a: Array[Byte]): (Http.Headers, Http.Request.Body.Singlepart) =
            (Chain.empty, Http.Request.Body.Singlepart(Http.Payload.Strict(a)))
          override def decodeWithRemainders(
              remainders: Http.Headers,
              payload: Array[Byte]
          ): Validated[Violations, (Http.Headers, Array[Byte])] = (remainders, payload).valid

  def apply[A, B, C](m: Method, a: Url[A], b: Headers[B], c: Request.Body[C]): Request[(A, B, C)] =
    new Request[(A, B, C)]:
      override def method: Method = m
      override def url: Url[A] = a
      override def headers: Headers[B] = b
      override def body: Body[C] = c
      override def decode(request: Http.Request): Validated[Violations, (A, B, C)] = Validated
        .cond(
          method === request.method,
          (),
          Violations.oneNec(
            History.Root / "method",
            Violation(Constraint.Equals(method.toString), OpenApi.Text(request.method.toString))
          )
        )
        .andThen(_ => url.decode(request.url))
        .andThen(a => headers.decodeWithRemainders(request.headers).map(_.tupleLeft(a)))
        .andThen { case (headers, (a, b)) => body.decode(headers, request.body).map(c => (a, b, c)) }
      override def encode(abc: (A, B, C)): Http.Request =
        val (additionalHeaders, body) = this.body.encode(abc._3)
        Http.Request(method, url.encode(abc._1), headers.encode(abc._2) ++ additionalHeaders, body)
