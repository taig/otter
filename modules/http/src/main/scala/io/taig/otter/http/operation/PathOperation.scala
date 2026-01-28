package io.taig.otter.http.operation

import io.taig.otter.http.Http
import io.taig.otter.InvariantK

trait PathOperation[F[_]]:
  self =>

  def empty: F[Unit]

  def lift[A](segment: Http.Segment[A]): F[A]

  def mapK[G[_]](fK: [A] => F[A] => G[A]): PathOperation[G] = new PathOperation[G]:
    override def empty: G[Unit] = fK(self.empty)

    override def lift[A](segment: Http.Segment[A]): G[A] = fK(self.lift(segment))

object PathOperation:
  inline def apply[F[_]](using self: PathOperation[F]): PathOperation[F] = self

  given InvariantK[PathOperation]:
    extension [G[_]](self: PathOperation[G])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): PathOperation[H] = self.mapK(fK)

  trait Read[F[_]] extends PathOperation[F]:
    self =>

    def lift[A](segment: Http.Segment.Read[A]): F[A]

    final override def lift[A](segment: Http.Segment[A]): F[A] = lift(segment: Http.Segment.Read[A])

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): PathOperation.Read[G] = new Read[G]:
      override def empty: G[Unit] = fK(self.empty)

      override def lift[A](segment: Http.Segment.Read[A]): G[A] = fK(self.lift(segment))

  object Read:
    inline def apply[F[_]](using self: PathOperation.Read[F]): PathOperation.Read[F] = self

    given InvariantK[PathOperation.Read]:
      extension [G[_]](self: PathOperation.Read[G])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): Read[H] = self.mapK(fK)

  trait Write[F[_]] extends PathOperation[F]:
    self =>

    def lift[A](segment: Http.Segment.Write[A]): F[A]

    final override def lift[A](segment: Http.Segment[A]): F[A] = lift(segment: Http.Segment.Write[A])

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): PathOperation.Write[G] = new Write[G]:
      override def empty: G[Unit] = fK(self.empty)

      override def lift[A](segment: Http.Segment.Write[A]): G[A] = fK(self.lift(segment))

  object Write:
    inline def apply[F[_]](using self: PathOperation.Write[F]): PathOperation.Write[F] = self

    given InvariantK[PathOperation.Write]:
      extension [G[_]](self: PathOperation.Write[G])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): Write[H] = self.mapK(fK)
