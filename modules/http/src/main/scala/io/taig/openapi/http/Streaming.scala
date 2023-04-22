package io.taig.openapi.http

import cats.ApplicativeThrow
import cats.effect.IO
import cats.syntax.all
import fs2.{Chunk, Pure, Stream}
import scodec.bits.ByteVector

abstract class Streaming[+A]:
  def toStream[F[_]: ApplicativeThrow]: Stream[F, A]

object Streaming:
  def of[F[_], A](data: Stream[F, A]): Streaming[A] = new Streaming[A]:
    override def toStream[G[_]](using G: ApplicativeThrow[G]): Stream[G, A] =
      try data.asInstanceOf[Stream[G, A]]
      catch {
        case _: ClassCastException =>
          Stream.raiseError[G](new IllegalStateException("Effect type mismatch"))
      }

  def pure(data: ByteVector): Streaming[Byte] = new Streaming[Byte]:
    override def toStream[F[_]: ApplicativeThrow]: Stream[Pure, Byte] = Stream.chunk(Chunk.byteVector(data))

  val Empty: Streaming[Nothing] = new Streaming[Nothing]:
    override def toStream[F[_]: ApplicativeThrow]: Stream[F, Nothing] = Stream.empty
