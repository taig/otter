package io.taig.otter.http

final class Fs2Stream(val isEmpty: Boolean) extends Stream

object Fs2Stream:
  def apply[F[_]](data: fs2.Stream[F, Byte]): F[Stream] =
    // TODO peak into stream to check if empty
    ???
