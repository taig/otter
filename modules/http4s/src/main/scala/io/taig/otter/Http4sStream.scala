package io.taig.otter

import cats.effect.Concurrent
import fs2.Pull
import io.taig.otter.http.Stream

final class Http4sStream[F[_], A](val isEmpty: Boolean, val toFs2: fs2.Stream[F, A]) extends Stream[A]

object Http4sStream:
  def apply[F[_]: Concurrent, A](data: fs2.Stream[F, A]): F[Stream[A]] = data.pull.peek1
    .flatMap:
      case Some((_, tail)) => Pull.output1(new Http4sStream(isEmpty = false, tail))
      case None            => Pull.output1(new Http4sStream(isEmpty = true, fs2.Stream.empty))
    .stream
    .head
    .compile
    .lastOrError
