package io.taig.openapi.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.openapi.OpenApi
import io.taig.openapi.http.Input.Body
import io.taig.openapi.http.Request.Body
import io.taig.openapi.http.Request.Body.Singlepart
import io.taig.openapi.schema.{Violations, Void}
import io.taig.openapi.validation.Constraint

sealed abstract class Input[A]:
  def method: Method
  def url: Url[?]
  def headers: Headers[?]
  def body: Input.Body[?]
  final def imap[B](f: A => B)(g: B => A): Input[B] = Input.Modify(this, f, g)
  def decode(request: Request): Validated[Violations, A]
  def encode(a: A): Request

object Input:
  sealed abstract class Body[A]:
    def decode(body: Request.Body): Validated[Violations, A]
    def encode(a: A): Request.Body

  object Body:
    abstract class Singlepart[A] extends Body[A]:
      final def optional: Body[Option[A]] = Singlepart.Optional(this)
      final override def decode(body: Request.Body): Validated[Violations, A] = body match
        case body: Request.Body.Singlepart => decode(body)
        case _: Request.Body.Multipart =>
          val violation = Constraint
            .tpe(OpenApi.fromString("Request.Body.Singlepart"))
            .toViolation(OpenApi.fromString("Request.Body.Multipart"))
          Violations.rootNec(violation).invalid
      def decode(body: Request.Body.Singlepart): Validated[Violations, A]
      override def encode(a: A): Request.Body.Singlepart

    object Singlepart:
      final private case class Optional[A](body: Input.Body.Singlepart[A]) extends Input.Body.Singlepart[Option[A]]:
        override def decode(body: Request.Body.Singlepart): Validated[Violations, Option[A]] = body match
          case Request.Body.Singlepart.Strict(data) =>
            if data.isEmpty then none[A].valid else this.body.decode(body).map(_.some)
          case Request.Body.Singlepart.Streaming(data) =>
            if data.isEmpty then none[A].valid else this.body.decode(body).map(_.some)
        override def encode(a: Option[A]): Request.Body.Singlepart = a.fold(Request.Body.Singlepart.Empty)(body.encode)

      val Empty: Input.Body.Singlepart[Void] = new Singlepart[Void]:
        override def decode(body: Request.Body.Singlepart): Validated[Violations, Void] = Void.valid
        override def encode(a: Void): Request.Body.Singlepart = Request.Body.Singlepart.Empty

      val Strict: Input.Body.Singlepart[Array[Byte]] = new Singlepart[Array[Byte]]:
        override def decode(body: Request.Body.Singlepart): Validated[Violations, Array[Byte]] = body match
          case Request.Body.Singlepart.Strict(data) => data.valid
          case _: Request.Body.Singlepart.Streaming =>
            val violation = Constraint
              .tpe(OpenApi.fromString("Request.Body.Singlepart.Strict"))
              .toViolation(OpenApi.fromString("Request.Body.Singlepart.Streaming"))
            Violations.rootNec(violation).invalid
        override def encode(as: Array[Byte]): Request.Body.Singlepart = Request.Body.Singlepart.Strict(as)

      val Streaming: Input.Body.Singlepart[Stream[Byte]] = new Singlepart[Stream[Byte]]:
        override def decode(body: Request.Body.Singlepart): Validated[Violations, Stream[Byte]] = body match
          case Request.Body.Singlepart.Strict(data)    => Stream.from(data).valid
          case Request.Body.Singlepart.Streaming(data) => data.valid
        override def encode(a: Stream[Byte]): Request.Body.Singlepart = Request.Body.Singlepart.Streaming(a)

    abstract class Multipart[A] extends Input.Body[A]:
      override def decode(body: Request.Body): Validated[Violations, A] = body match
        case body: Request.Body.Multipart => decode(body)
        case _: Request.Body.Singlepart =>
          val violation = Constraint
            .tpe("Request.Body.Multipart")
            .toViolation(OpenApi.fromString("Request.Body.Singlepart"))
            .mapReference(OpenApi.fromString)
          Violations.rootNec(violation).invalid
      def decode(body: Request.Body.Multipart): Validated[Violations, A]

  final private case class Root[A, B, C](method: Method, url: Url[A], headers: Headers[B], body: Body[C])
      extends Input[(A, B, C)]:
    override def decode(request: Request): Validated[Violations, (A, B, C)] = ???
    override def encode(abc: (A, B, C)): Request =
      val (path, queries) = url.encode(abc._1)
      Request(method, path, queries, headers.encode(abc._2), body.encode(abc._3))

  final private case class Modify[A, B](input: Input[A], f: A => B, g: B => A) extends Input[B]:
    export input.{body, headers, method, url}
    override def decode(request: Request): Validated[Violations, B] = input.decode(request).map(f)
    override def encode(b: B): Request = input.encode(g(b))

  transparent inline def apply[A, B, C](
      method: Method,
      url: Url[A],
      headers: Headers[B],
      body: Body[C]
  ): Input[?] = inline (url, headers, body) match
    case (url: Url[Void], headers: Headers[Void], body) =>
      Root(method, url, headers, body).imap { case (_, _, c) => c }(c => (Void, Void, c))
    case (url: Url[Void], headers, body: Body[Void]) =>
      Root(method, url, headers, body).imap { case (_, b, _) => b }(b => (Void, b, Void))
    case (url, headers: Headers[Void], body: Body[Void]) =>
      Root(method, url, headers, body).imap { case (a, _, _) => a }(a => (a, Void, Void))
    // TODO more cases
    case _ => Root(method, url, headers, body)
