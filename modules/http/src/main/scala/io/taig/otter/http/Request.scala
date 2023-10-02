package io.taig.otter.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.{Data, Schema}
import io.taig.otter.schemas.*
import io.taig.otter.http.Http.Request.Body
import io.taig.otter.http.Http.{Payload, Request}
import io.taig.otter.validation.{Constraint, History, Violation, Violations}

sealed abstract class Request[A](val description: Option[String]):
  self =>
  def method: Method
  def url: Url[?]
  def headers: Headers[?]
  def body: Request.Body[?]

  def description(f: Option[String] => Option[String]): Request[A] = Request(this, f(description))
  def description(value: Option[String]): Request[A] = description(_ => value)
  def description(value: String): Request[A] = description(Some(value))

  final def matches(method: Method, url: Http.Url): Boolean = this.method === method && this.url.matches(url)

  final def andThen[B](f: A => Validated[Violations, B])(g: B => A): Request[B] = new Request[B](description):
    export self.{body, headers, method, url}
    override def decode(request: Http.Request): Validated[Violations, B] = self.decode(request).andThen(f)
    override def encode(b: B): Http.Request = self.encode(g(b))

  final def imap[B](f: A => B)(g: B => A): Request[B] = andThen(f(_).valid)(g)

  final def zip[B](other: Headers[B]): Request[(A, B)] = new Request[(A, B)](description):
    export self.{body, method, url}
    override def headers: Headers[?] = self.headers.zip(other)
    override def decode(request: Http.Request): Validated[Violations, (A, B)] =
      self
        .decode(request)
        .andThen(a => other.decode(request.headers).leftMap(_.modifyHistory("headers" /: _)).map((a, _)))

    override def encode(ab: (A, B)): Http.Request = self.encode(ab._1).modifyHeaders(_ ++ other.encode(ab._2))

  def decode(request: Http.Request): Validated[Violations, A]
  def encode(a: A): Http.Request

object Request:
  sealed abstract class Body[A]:
    self =>
    type Self[a] <: Body[a] { type Self[a] = self.Self[a] }

    def isEmpty: Boolean

//    def andThen[B](f: A => Validated[Violations, B])(g: B => A): Self[B]
//    final def imap[B](f: A => B)(g: B => A): Self[B] = andThen(f(_).valid)(g)
//    def zip[B](headers: Headers[B]): Self[(A, B)]

    def decode(headers: Http.Headers, body: Http.Request.Body): Validated[Violations, A]
    def encode(a: A): (Http.Headers, Http.Request.Body)

  object Body extends ToRequestBodyOps:
    sealed abstract class Singlepart[A] extends Request.Body[A]:
      self =>

      override type Self[a] <: Request.Body.Singlepart[a] { type Self[a] = self.Self[a] }

      override def decode(headers: Http.Headers, body: Http.Request.Body): Validated[Violations, A] = body match
        case Http.Request.Body.Singlepart(payload) => decode(headers, payload)
        case Http.Request.Body.Multipart()         => ???
      def decode(headers: Http.Headers, payload: Http.Payload): Validated[Violations, A]
      override def encode(a: A): (Http.Headers, Http.Request.Body.Singlepart)

    object Singlepart:
      sealed abstract class Strict[A](val schema: Schema[?]) extends Request.Body.Singlepart[A]:
        self =>
        final override type Self[a] = Request.Body.Singlepart.Strict[a]

        final override def decode(headers: Http.Headers, payload: Http.Payload): Validated[Violations, A] =
          payload match
            case Payload.Strict(data) => decode(headers, data)
            case Payload.Streaming(_) => Violations.rootNec(Violation.tpe("strict", "streaming")).invalid
        def decode(headers: Http.Headers, payload: Array[Byte]): Validated[Violations, A]
        override def encode(a: A): (Http.Headers, Http.Request.Body.Singlepart)

      object Strict:
        val Empty: Request.Body.Singlepart.Strict[Unit] = new Strict[Unit](dynamic.empty):
          override def isEmpty: Boolean = true
          override def decode(headers: Http.Headers, payload: Array[Byte]): Validated[Violations, Unit] = ().valid
          override def encode(a: Unit): (Http.Headers, Http.Request.Body.Singlepart) =
            (Chain.empty, Http.Request.Body.Singlepart(Http.Payload.Strict(Array.emptyByteArray)))

        val Binary: Request.Body.Singlepart.Strict[Array[Byte]] = new Strict[Array[Byte]](dynamic.empty):
          override def isEmpty: Boolean = false
          override def decode(headers: Http.Headers, payload: Array[Byte]): Validated[Violations, Array[Byte]] =
            payload.valid
          override def encode(a: Array[Byte]): (Http.Headers, Http.Request.Body.Singlepart) =
            (Chain.empty, Http.Request.Body.Singlepart(Http.Payload.Strict(a)))

        def apply[A](
            f: (Http.Headers, Array[Byte]) => Validated[Violations, Data],
            g: Data => (Http.Headers, Array[Byte]),
            of: Schema[A]
        ): Request.Body.Singlepart.Strict[A] = new Strict[A](of):
          override def isEmpty: Boolean = false
          override def decode(headers: Http.Headers, payload: Array[Byte]): Validated[Violations, A] =
            f(headers, payload).andThen(of.decode)
          override def encode(a: A): (Http.Headers, Http.Request.Body.Singlepart) =
            g(of.encode(a)).map(bytes => Http.Request.Body.Singlepart(Http.Payload.Strict(bytes)))

  def apply[A](request: Request[A], description: Option[String]): Request[A] =
    new Request[A](description) { export request.* }

  def apply[A, B](m: Method, a: Url[A], b: Request.Body[B]): Request[(A, B)] = new Request[(A, B)](None):
    override def method: Method = m
    override def url: Url[A] = a
    override def headers: Headers[Unit] = Headers.Empty
    override def body: Body[B] = b
    override def decode(request: Http.Request): Validated[Violations, (A, B)] = Validated
      .cond(
        method === request.method,
        (),
        Violations.oneNec(
          History.Root / "method",
          Violation(Constraint.Equals(method.toString), Data.String(request.method.toString))
        )
      )
      .andThen(_ => url.decode(request.url).leftMap(_.modifyHistory("url" /: _)))
      .andThen(body.decode(request.headers, request.body).leftMap(_.modifyHistory("body" /: _)).tupleLeft)
    override def encode(ab: (A, B)): Http.Request =
      val (additionalHeaders, body) = this.body.encode(ab._2)
      Http.Request(method, url.encode(ab._1), additionalHeaders, body)
