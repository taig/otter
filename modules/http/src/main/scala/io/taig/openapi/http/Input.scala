package io.taig.openapi.http

import cats.data.Validated
import cats.effect.Async
import cats.syntax.all.*
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

object Input:
  sealed abstract class Body[A[_[_]]]:
    def decode[F[_]: Async](body: Request.Body[F]): F[Validated[Violations, A[F]]]
    def encode[F[_]: Async](a: A[F]): Request.Body[F]

  object Body:
    type Strict[A] = [_[_]] =>> A
    type Streaming[A] = [F[_]] =>> Stream[F, A]

    abstract class Singlepart[A[_[_]]] extends Body[A]:
      final def optional: Body[[F[_]] =>> Option[A[F]]] = Singlepart.Optional(this)
      final override def decode[F[_]: Async](body: Request.Body[F]): F[Validated[Violations, A[F]]] = body match
        case body: Request.Body.Singlepart[?] => decode(body)
        case _: Request.Body.Multipart[?] =>
          val violation = Constraint
            .tpe(OpenApi.fromString("Request.Body.Singlepart"))
            .toViolation(OpenApi.fromString("Request.Body.Multipart"))
          Violations.rootNec(violation).invalid.pure[F]
      def decode[F[_]: Async](body: Request.Body.Singlepart[F]): F[Validated[Violations, A[F]]]
      override def encode[F[_]: Async](a: A[F]): Request.Body.Singlepart[F]

    object Singlepart:
      object Empty extends Input.Body.Singlepart[Input.Body.Strict[Void]]:
        override def decode[F[_]: Async](body: Request.Body.Singlepart[F]): F[Validated[Violations, Void]] =
          Void.valid.pure[F]
        override def encode[F[_]: Async](a: Void): Request.Body.Singlepart[F] = Request.Body.Singlepart.Empty

      object Strict extends Input.Body.Singlepart[Input.Body.Strict[Array[Byte]]]:
        override def decode[F[_]: Async](body: Request.Body.Singlepart[F]): F[Validated[Violations, Array[Byte]]] =
          body.data.compile.to(Array).map(_.valid)
        override def encode[F[_]: Async](a: Array[Byte]): Request.Body.Singlepart[F] =
          Request.Body.Singlepart(Stream.chunk(Chunk.array(a)))

      object Streaming extends Input.Body.Singlepart[Input.Body.Streaming[Byte]]:
        override def decode[F[_]: Async](body: Request.Body.Singlepart[F]): F[Validated[Violations, Stream[F, Byte]]] =
          body.data.valid.pure[F]
        override def encode[F[_]: Async](a: Stream[F, Byte]): Request.Body.Singlepart[F] = Request.Body.Singlepart(a)

      final private case class Optional[A[_[_]]](body: Body.Singlepart[A])
          extends Input.Body.Singlepart[[F[_]] =>> Option[A[F]]] {
        override def decode[F[_]: Async](body: Request.Body.Singlepart[F]): F[Validated[Violations, Option[A[F]]]] =
          // TODO peek into body stream to check whether it's empty
          ???
        override def encode[F[_]: Async](a: Option[A[F]]): Request.Body.Singlepart[F] =
          a.fold(Request.Body.Singlepart.Empty)(body.encode)
      }
//
////    abstract class Multipart[A] extends Input.Body[A]:
////      override def decode[F[_]: Async](body: Request.Body[F]): Validated[Violations, Effect[F, A]] = body match
////        case body: Request.Body.Multipart[F] => decode(body)
////        case _: Request.Body.Singlepart[F] =>
////          val violation = Constraint
////            .tpe("Request.Body.Multipart")
////            .toViolation(OpenApi.fromString("Request.Body.Singlepart"))
////            .mapReference(OpenApi.fromString)
////
////          Violations.rootNec(violation).invalid
////      def decode[F[_]: Async](body: Request.Body.Multipart[F]): Validated[Violations, Effect[F, A]]
