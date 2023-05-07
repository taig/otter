package io.taig.openapi.http

import cats.data.Validated
import cats.effect.{Async, Concurrent, IO, LiftIO}
import cats.syntax.all.*
import cats.{~>, Applicative}
import fs2.{Chunk, Stream}
import io.taig.openapi.OpenApi
import io.taig.openapi.http.Input.Body
import io.taig.openapi.http.Request.Body
import io.taig.openapi.schema.{Violations, Void}
import io.taig.openapi.validation.Constraint

sealed abstract class Input[A]:
  def method: Method
  def url: Url[?]
  def headers: Headers[?]
  def body: Input.Body[?]

  def encode(a: A): Request

object Input:
  sealed abstract class Body[A]:
    def decode[F[_]: Concurrent: LiftIO](body: Request.Body): F[Validated[Violations, A]]
    def encode(a: A): Request.Body

  object Body:
    abstract class Singlepart[A] extends Body[A]:
      final def optional: Body[Option[A]] = Singlepart.Optional(this)
      final override def decode[F[_]: Concurrent: LiftIO](body: Request.Body): F[Validated[Violations, A]] =
        body match
          case body: Request.Body.Singlepart => decode(body)
          case _: Request.Body.Multipart =>
            val violation = Constraint
              .tpe(OpenApi.fromString("Request.Body.Singlepart"))
              .toViolation(OpenApi.fromString("Request.Body.Multipart"))
            Violations.rootNec(violation).invalid.pure[F]
      def decode[F[_]: Concurrent: LiftIO](body: Request.Body.Singlepart): F[Validated[Violations, A]]
      override def encode(a: A): Request.Body.Singlepart

    object Singlepart:
      final private case class Optional[A](body: Input.Body.Singlepart[A]) extends Input.Body.Singlepart[Option[A]]:
        override def decode[F[_]: Concurrent: LiftIO](
            body: Request.Body.Singlepart
        ): F[Validated[Violations, Option[A]]] =
          // TODO peek into body stream to check whether it's empty
          ???
        override def encode(a: Option[A]): Request.Body.Singlepart = a.fold(Request.Body.Singlepart.Empty)(body.encode)

      val empty: Input.Body.Singlepart[Void] = new Singlepart[Void]:
        override def decode[F[_]: Concurrent: LiftIO](body: Request.Body.Singlepart): F[Validated[Violations, Void]] =
          Void.valid.pure[F]
        override def encode(a: Void): Request.Body.Singlepart = Request.Body.Singlepart(Streaming.empty)

      val strict: Input.Body.Singlepart[Array[Byte]] = new Singlepart[Array[Byte]]:
        override def decode[F[_]: Concurrent: LiftIO](
            body: Request.Body.Singlepart
        ): F[Validated[Violations, Array[Byte]]] = body.data.toArray.map(_.valid)
        override def encode(as: Array[Byte]): Request.Body.Singlepart = Request.Body.Singlepart(Streaming.from(as))

      val streaming: Input.Body.Singlepart[Streaming[Byte]] = new Singlepart[Streaming[Byte]]:
        override def decode[F[_]: Concurrent: LiftIO](
            body: Request.Body.Singlepart
        ): F[Validated[Violations, Streaming[Byte]]] = body.data.valid.pure[F]
        override def encode(a: Streaming[Byte]): Request.Body.Singlepart = Request.Body.Singlepart(a)

//////    abstract class Multipart[A] extends Input.Body[A]:
//////      override def decode[F[_]: Concurrent](body: Request.Body[F]): Validated[Violations, Effect[F, A]] = body match
//////        case body: Request.Body.Multipart[F] => decode(body)
//////        case _: Request.Body.Singlepart[F] =>
//////          val violation = Constraint
//////            .tpe("Request.Body.Multipart")
//////            .toViolation(OpenApi.fromString("Request.Body.Singlepart"))
//////            .mapReference(OpenApi.fromString)
//////
//////          Violations.rootNec(violation).invalid
//////      def decode[F[_]: Concurrent](body: Request.Body.Multipart[F]): Validated[Violations, Effect[F, A]]

  final private case class Root[A, B, C](method: Method, url: Url[A], headers: Headers[B], body: Body[C])
      extends Input[(A, B, C)]:
    override def encode(abc: (A, B, C)): Request =
      val (path, queries) = url.encode(abc._1)
      Request(method, path, queries, headers.encode(abc._2), body.encode(abc._3))

//  transparent inline def apply[A, B, C](
//      method: Method,
//      url: Url[A],
//      headers: Headers[B],
//      body: Body[C]
//  ): Input[?] = ???
