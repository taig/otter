package io.taig.openapi.http

import cats.data.Validated
import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.{Chunk, Stream}
import io.taig.openapi.Encoder
import io.taig.openapi.schema.applyValidation
import io.taig.openapi.schema.{Violations, Void}
import io.taig.openapi.validation.Validation

object Output:
  abstract class Body[A]:
    def decode[F[_]: Concurrent](body: Stream[F, Byte]): F[Validated[Violations, A]]
    def encode[F[_]: Concurrent](a: A): Stream[F, Byte]

  object Body:
    final private case class Validate[A, B: Encoder, C](
        body: Output.Body[A],
        validation: Validation[B, A, A, C],
        g: C => A
    ) extends Body[C] {
      override def decode[F[_]: Concurrent](body: Stream[F, Byte]): F[Validated[Violations, C]] =
        this.body.decode(body).map(_.andThen(applyValidation(validation, ???)))
      override def encode[F[_]: Concurrent](c: C): Stream[F, Byte] = body.encode(g(c))
    }
    final private case class Optional[A](body: Body[A]) extends Body[Option[A]] {
      override def decode[F[_]: Concurrent](body: Stream[F, Byte]): F[Validated[Violations, Option[A]]] =
        // TODO peek into stream to check if empty
        ???
      override def encode[F[_]: Concurrent](a: Option[A]): Stream[F, Byte] = a.fold(Stream.empty)(body.encode)
    }

    val Empty: Output.Body[Void] = new Body[Void]:
      override def decode[F[_]: Concurrent](body: Stream[F, Byte]): F[Validated[Violations, Void]] =
        Void.valid.pure[F]
      override def encode[F[_]: Concurrent](a: Void): Stream[F, Byte] = Stream.empty

    val Strict: Output.Body[Array[Byte]] = new Body[Array[Byte]]:
      override def decode[F[_]: Concurrent](body: Stream[F, Byte]): F[Validated[Violations, Array[Byte]]] =
        body.compile.to(Array).map(_.valid)
      override def encode[F[_]: Concurrent](a: Array[Byte]): Stream[F, Byte] = Stream.chunk(Chunk.array(a))
