package io.taig.openapi.http

import cats.ApplicativeThrow
import fs2.{Chunk, Pure, Stream}
import scodec.bits.ByteVector

abstract class ToStream[+A]:
  def toStream[F[_]: ApplicativeThrow]: Stream[F, A]

object ToStream:
  def of[F[_], A](data: Stream[F, A]): ToStream[A] = new ToStream[A]:
    override def toStream[G[_]](using G: ApplicativeThrow[G]): Stream[G, A] =
      try data.asInstanceOf[Stream[G, A]]
      catch
        case _: ClassCastException =>
          Stream.raiseError[G](new IllegalStateException("Effect type mismatch"))

  def pure(data: ByteVector): ToStream[Byte] = new ToStream[Byte]:
    override def toStream[F[_]: ApplicativeThrow]: Stream[Pure, Byte] = Stream.chunk(Chunk.byteVector(data))

  val Empty: ToStream[Nothing] = new ToStream[Nothing]:
    override def toStream[F[_]: ApplicativeThrow]: Stream[F, Nothing] = Stream.empty
