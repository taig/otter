package io.taig.otter

import cats.Invariant

trait CodecInvariant[F[_]] extends Invariant[F]:
  extension [A](self: F[A])
    final def to[B](using convert: Convert[A, B]): F[B] =
      imap(self)(convert.to)(convert.from)
