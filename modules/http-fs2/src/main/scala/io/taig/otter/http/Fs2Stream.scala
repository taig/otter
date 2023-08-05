package io.taig.otter.http

import cats.effect.Concurrent
import fs2.Pull

final class Fs2Stream[F[_]](val isEmpty: Boolean, val toFs2: fs2.Stream[F, Byte]) extends Stream

object Fs2Stream:
  def apply[F[_]: Concurrent](data: fs2.Stream[F, Byte]): F[Stream] = data.pull.peek1
    .flatMap {
      case Some((_, tail)) => Pull.output1(new Fs2Stream(isEmpty = false, tail))
      case None            => Pull.output1(new Fs2Stream(isEmpty = true, fs2.Stream.empty))
    }
    .stream
    .head
    .compile
    .lastOrError
