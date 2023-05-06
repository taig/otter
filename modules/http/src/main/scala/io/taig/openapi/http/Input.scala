package io.taig.openapi.http

import cats.{~>, catsInstancesForId, Functor}
import cats.data.Validated
import cats.effect.std.Dispatcher
import cats.effect.{Async, IO, LiftIO}
import cats.syntax.all.*
import io.taig.openapi.OpenApi
import io.taig.openapi.http.Request.Body
import io.taig.openapi.schema.{Violations, Void}
import io.taig.openapi.validation.Constraint
import fs2.{Chunk, Stream}
import io.taig.openapi.http.Input.Body

sealed abstract class Input[A]:
  def method: Method
  def url: Url[?]
  def headers: Headers[?]
  def body: Input.Body[?]

object Input:
  sealed abstract class Body[A[_[_]]]:
    final def optional: Body[[F[_]] =>> Option[A[F]]] = ??? // Body.Optional(this)
    def decode[F[_]: Async](body: Request.Body.Singlepart[F]): F[Validated[Violations, A[F]]]
    def encode[F[_]: Async](a: A[F]): Request.Body.Singlepart[F]

  object Body:
    type Strict[A] = [_[_]] =>> A
    type Streaming[A] = [F[_]] =>> Stream[F, A]

    object Singlepart:
      object Empty extends Input.Body[Input.Body.Strict[Void]]:
        override def decode[F[_]: Async](body: Request.Body.Singlepart[F]): F[Validated[Violations, Void]] =
          Void.valid.pure[F]
        override def encode[F[_]: Async](a: Void): Request.Body.Singlepart[F] = Request.Body.Singlepart.Empty

      object Strict extends Input.Body[Input.Body.Strict[Array[Byte]]]:
        override def decode[F[_]: Async](body: Request.Body.Singlepart[F]): F[Validated[Violations, Array[Byte]]] = ???
        override def encode[F[_]: Async](a: Array[Byte]): Request.Body.Singlepart[F] = ???

      object Streaming extends Body[Input.Body.Streaming[Byte]]:
        override def decode[F[_]: Async](body: Request.Body.Singlepart[F]): F[Validated[Violations, Stream[F, Byte]]] =
          body.data.valid.pure[F]
        override def encode[F[_]: Async](a: Stream[F, Byte]): Request.Body.Singlepart[F] = Request.Body.Singlepart(a)
//    abstract class Singlepart[A] extends Input.Body[A]
////      final override def decode(body: Request.Body[Effect])(using Async[Effect]): Effect[Validated[Violations, A]] =
////        body match
////          case body: Request.Body.Singlepart[Effect] => decode(body)
////          case _: Request.Body.Multipart[Effect] =>
////            val violation = Constraint
////              .tpe("Request.Body.Singlepart")
////              .toViolation(OpenApi.fromString("Request.Body.Multipart"))
////              .mapReference(OpenApi.fromString)
////
////            Violations.rootNec(violation).invalid.pure[Effect]
////      def decode(body: Request.Body.Singlepart[Effect])(using Async[Effect]): Effect[Validated[Violations, A]]
////      override def encode(a: A)(using Async[Effect]): Request.Body.Singlepart[Effect]
//
//    object Singlepart:
//      object Empty extends Input.Body.Singlepart[Void] {
//        override def codec[F[_]: Async]: Codec[F, Void] = new Codec[F, Void] {
//          override def decode(body: Request.Body.Singlepart[F]): F[Validated[Violations, Void]] = Void.valid.pure[F]
//          override def encode(a: Void): Request.Body.Singlepart[F] = Request.Body.Singlepart.Empty
//        }
//      }
////      object Empty extends Input.Body.Singlepart[Void] {
////        override def decode(body: Request.Body.Singlepart[Effect])(using
////            Async[Effect]
////        ): Effect[Validated[Violations, Void]] = Void.valid.pure[Effect]
////        override def encode(a: Void)(using Async[Effect]): Request.Body.Singlepart[Effect] = Request.Body.Singlepart.Empty
////      }
//
//      object Strict extends Input.Body.Singlepart[Array[Byte]] {
//        override def codec[F[_]: Async]: Codec[F, Array[Byte]] = new Codec[F, Array[Byte]] {
//          override def decode(body: Request.Body.Singlepart[F]): F[Validated[Violations, Array[Byte]]] =
//            body.data.compile.to(Array).map(_.valid)
//          override def encode(a: Array[Byte]): Request.Body.Singlepart[F] =
//            Request.Body.Singlepart(Stream.chunk(Chunk.array(a)))
//        }
//      }
////      object Strict extends Input.Body.Singlepart[Array[Byte]] {
////        override def decode(body: Request.Body.Singlepart[Effect])(using
////            Async[Effect]
////        ): Effect[Validated[Violations, Array[Byte]]] = body.data.compile.to(Array).map(_.valid)
////        override def encode(a: Array[Byte])(using Async[Effect]): Request.Body.Singlepart[Effect] =
////          Request.Body.Singlepart(Stream.chunk(Chunk.array(a)))
////      }
//
////      object Strict extends Input.Body.Singlepart[Array[Byte]]:
////        override def decode[F[_]: Async](
////            body: Request.Body.Singlepart[F]
////        ): F[Validated[Violations, Array[Byte]]] = body.data.compile.to(Array).map(_.valid)
////        override def encode[F[_]: LiftIO](a: Array[Byte]): Request.Body.Singlepart[F] =
////          Request.Body.Singlepart(Stream.chunk(Chunk.array(a)))
//
//      object Streaming extends Input.Body.Singlepart[StreamWrapper[Byte]] {
//        override def codec[F[_]: Async]: Codec[F, StreamWrapper[Byte] { type Effect[a] = F[a] }] = ???
//      }
////      object Streaming extends Input.Body.Singlepart[StreamWrapper[Byte]] {
////        override def decode[F[_]: Async](
////            body: Request.Body.Singlepart[F]
////        ): F[Validated[Violations, StreamWrapper[Byte]]] = new StreamWrapper[Byte] {
////            override def toStream[F[_]]: Stream[F, Byte] = ???
////          }.valid.pure[F]
////
////        override def encode[F[_]: LiftIO](a: StreamWrapper[Byte]): Request.Body.Singlepart[F] =
////          Request.Body.Singlepart(a.toStream)
////      }
//
////    final private case class Optional[A](body: Body[A]) extends Input.Body[Option[A]]:
////      override def decode[F[_]: Async](body: Request.Body[F]): F[Validated[Violations, Option[A]]] = ???
////      override def encode[F[_]: LiftIO](a: Option[A]): Request.Body[F] =
////        a.fold(Request.Body.Singlepart.Empty)(body.encode)
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
