package io.taig.openapi.http

import cats.Applicative
import cats.data.Validated
import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.{Chunk, Stream}
import io.taig.openapi.OpenApi
import io.taig.openapi.http.Request.Body
import io.taig.openapi.schema.{Violations, Void}
import io.taig.openapi.validation.Constraint
import scodec.bits.ByteVector

object Input:
  abstract class Body[A]:
    type Effect[f[_], a]
    def optional: Body[Option[A]] = Body.Optional(this)
    def decode[F[_]: Concurrent](body: Request.Body[F]): Validated[Violations, Effect[F, A]]
    def encode[F[_]: Applicative](a: Effect[F, A]): F[Request.Body[F]]

  object Body:
    abstract class Singlepart[A] extends Input.Body[A]:
      final override def decode[F[_]: Concurrent](body: Request.Body[F]): Validated[Violations, Effect[F, A]] =
        body match
          case body: Request.Body.Singlepart[F] => decode(body)
          case _: Request.Body.Multipart[F] =>
            val violation = Constraint
              .tpe("Request.Body.Singlepart")
              .toViolation(OpenApi.fromString("Request.Body.Multipart"))
              .mapReference(OpenApi.fromString)

            Violations.rootNec(violation).invalid

      def decode[F[_]: Concurrent](body: Request.Body.Singlepart[F]): Validated[Violations, Effect[F, A]]

    object Singlepart:
      object Empty extends Input.Body.Singlepart[Void]:
        override type Effect[_[_], a] = a
        override def decode[F[_]: Concurrent](body: Request.Body.Singlepart[F]): Validated[Violations, Void] =
          Void.valid
        override def encode[F[_]: Applicative](a: Void): F[Request.Body[F]] = Request.Body.Singlepart.Empty.pure[F]

      object Strict extends Input.Body.Singlepart[ByteVector]:
        override type Effect[f[_], a] = f[a]
        override def decode[F[_]: Concurrent](body: Request.Body.Singlepart[F]): Validated[Violations, F[ByteVector]] =
          body.data.compile.to(ByteVector).valid
        override def encode[F[_]: Applicative](a: F[ByteVector]): F[Request.Body[F]] =
          Request.Body.Singlepart(Stream.eval(a).flatMap(a => Stream.chunk(Chunk.byteVector(a)))).pure[F]

      object Streaming extends Input.Body.Singlepart[Byte]:
        override type Effect[f[_], a] = Stream[f, a]
        override def decode[F[_]: Concurrent](
            body: Request.Body.Singlepart[F]
        ): Validated[Violations, Stream[F, Byte]] = body.data.valid
        override def encode[F[_]: Applicative](a: Stream[F, Byte]): F[Request.Body[F]] =
          Request.Body.Singlepart(a).pure[F]

    final private case class Optional[A](body: Body[A]) extends Input.Body[Option[A]]:
      override type Effect[f[_], a] = body.Effect[f, a]
      override def decode[F[_]: Concurrent](body: Request.Body[F]): Validated[Violations, Effect[F, Option[A]]] = ???
      override def encode[F[_]: Applicative](a: Effect[F, Option[A]]): F[Request.Body[F]] = ???
