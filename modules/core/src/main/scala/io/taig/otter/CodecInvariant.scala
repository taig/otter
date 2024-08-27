package io.taig.otter

import cats.Invariant

trait CodecInvariant[F[_]] extends Invariant[F]:
  extension [A](self: F[A]) final def const(a: A): F[Unit] = imap(self)(_ => ())(_ => a)

  extension (self: F[Unit]) final def as[A](a: A): F[a.type] = imap(self)(_ => a)(_ => ())
