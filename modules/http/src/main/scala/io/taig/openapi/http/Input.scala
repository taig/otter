package io.taig.openapi.http

import cats.data.Validated
import cats.effect.{Async, Concurrent, IO, LiftIO}
import cats.syntax.all.*
import cats.~>
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

  def encode[F[_]: Concurrent](a: A): Request[F]

object Input:
  sealed abstract class Body[A]:
    def decode[F[_]: Concurrent](body: Request.Body[F]): F[Validated[Violations, A]]
    def encode[F[_]: Concurrent](a: A): Request.Body[F]

  object Body:
    abstract class Singlepart[A] extends Body[A]:
      final def optional: Body[Option[A]] = Singlepart.Optional(this)
      final override def decode[F[_]: Concurrent](body: Request.Body[F]): F[Validated[Violations, A]] =
        body match
          case body: Request.Body.Singlepart[?] => decode(body)
          case _: Request.Body.Multipart[?] =>
            val violation = Constraint
              .tpe(OpenApi.fromString("Request.Body.Singlepart"))
              .toViolation(OpenApi.fromString("Request.Body.Multipart"))
            Violations.rootNec(violation).invalid.pure[F]
      def decode[F[_]: Concurrent](body: Request.Body.Singlepart[F]): F[Validated[Violations, A]]
      override def encode[F[_]: Concurrent](a: A): Request.Body.Singlepart[F]

    object Singlepart:
      object Empty extends Input.Body.Singlepart[Void]:
        override def decode[F[_]: Concurrent](
            body: Request.Body.Singlepart[F]
        ): F[Validated[Violations, Void]] =
          Void.valid.pure[F]
        override def encode[F[_]: Concurrent](a: Void): Request.Body.Singlepart[F] =
          Request.Body.Singlepart.Empty

      object Strict extends Input.Body.Singlepart[Array[Byte]]:
        override def decode[F[_]: Concurrent](
            body: Request.Body.Singlepart[F]
        ): F[Validated[Violations, Array[Byte]]] =
          body.data.compile.to(Array).map(_.valid)
        override def encode[F[_]: Concurrent](a: Array[Byte]): Request.Body.Singlepart[F] =
          Request.Body.Singlepart(Stream.chunk(Chunk.array(a)))

      object Streaming extends Input.Body.Singlepart[ToStream[Byte]]:
        override def decode[F[_]: Concurrent](
            body: Request.Body.Singlepart[F]
        ): F[Validated[Violations, ToStream[Byte]]] = ToStream.of(body.data).valid.pure[F]
        override def encode[F[_]: Concurrent](a: ToStream[Byte]): Request.Body.Singlepart[F] =
          Request.Body.Singlepart(a.toStream)

      final private case class Optional[A](body: Body.Singlepart[A]) extends Input.Body.Singlepart[Option[A]] {
        override def decode[F[_]: Concurrent](
            body: Request.Body.Singlepart[F]
        ): F[Validated[Violations, Option[A]]] =
          // TODO peek into body stream to check whether it's empty
          ???
        override def encode[F[_]: Concurrent](a: Option[A]): Request.Body.Singlepart[F] =
          a.fold(Request.Body.Singlepart.Empty)(body.encode)
      }
//
////    abstract class Multipart[A] extends Input.Body[A]:
////      override def decode[F[_]: Concurrent](body: Request.Body[F]): Validated[Violations, Effect[F, A]] = body match
////        case body: Request.Body.Multipart[F] => decode(body)
////        case _: Request.Body.Singlepart[F] =>
////          val violation = Constraint
////            .tpe("Request.Body.Multipart")
////            .toViolation(OpenApi.fromString("Request.Body.Singlepart"))
////            .mapReference(OpenApi.fromString)
////
////          Violations.rootNec(violation).invalid
////      def decode[F[_]: Concurrent](body: Request.Body.Multipart[F]): Validated[Violations, Effect[F, A]]

  final private case class Root[A, B, C](method: Method, url: Url[A], headers: Headers[B], body: Body[C])
      extends Input[(A, B, C)]:
    override def encode[F[_]: Concurrent](abc: (A, B, C)): Request[F] =
      val (path, queries) = url.encode(abc._1)
      Request(method, path, queries, headers.encode(abc._2), body.encode(abc._3))

  transparent inline def apply[A, B, C](
      method: Method,
      url: Url[A],
      headers: Headers[B],
      body: Body[C]
  ): Input[?] = ???
