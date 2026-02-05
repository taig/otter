package io.taig.otter.http.operation

import io.taig.otter.InvariantK

trait PathOperation[F[_], G[_]]:
  self =>

  def empty: F[Unit]

  def lift[A](ga: G[A]): F[A]

  def mapK[H[_]](fK: [A] => F[A] => H[A]): PathOperation[H, G] = new PathOperation[H, G]:
    override def empty: H[Unit] = fK(self.empty)

    override def lift[A](ga: G[A]): H[A] = fK(self.lift(ga))

object PathOperation:
  trait Read[F[_], G[_]] extends PathOperation[F, G]:
    self =>

    override def mapK[H[_]](fK: [A] => F[A] => H[A]): PathOperation.Read[H, G] = new Read[H, G]:
      override def empty: H[Unit] = fK(self.empty)

      override def lift[A](segment: G[A]): H[A] = fK(self.lift(segment))

  object Read:
    inline def apply[F[_], G[_]](using self: PathOperation.Read[F, G]): PathOperation.Read[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> PathOperation.Read[f, F]]:
      extension [G[_]](self: PathOperation.Read[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): PathOperation.Read[H, F] =
          self.mapK(fK)

  trait Write[F[_], G[_]] extends PathOperation[F, G]:
    self =>

    override def mapK[H[_]](fK: [A] => F[A] => H[A]): PathOperation.Write[H, G] = new Write[H, G]:
      override def empty: H[Unit] = fK(self.empty)

      override def lift[A](segment: G[A]): H[A] = fK(self.lift(segment))

  object Write:
    inline def apply[F[_], G[_]](using self: PathOperation.Write[F, G]): PathOperation.Write[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> PathOperation.Write[f, F]]:
      extension [G[_]](self: PathOperation.Write[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): PathOperation.Write[H, F] =
          self.mapK(fK)

  inline def apply[F[_], G[_]](using self: PathOperation[F, G]): PathOperation[F, G] = self

  given [F[_]] => InvariantK[[f[_]] =>> PathOperation[F, f]] = ???