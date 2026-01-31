package io.taig.otter.http.operation

import io.taig.otter.InvariantK
import io.taig.otter.Reference

trait QueriesOperation[F[_], G[_]]:
  self =>

  def empty: F[Unit]

  def lift[A](query: Reference[G, A]): F[A]

  def mapK[H[_]](fK: [A] => F[A] => H[A]): QueriesOperation[H, G] = new QueriesOperation[H, G]:
    override def empty: H[Unit] = fK(self.empty)

    override def lift[A](query: Reference[G, A]): H[A] = fK(self.lift(query))

object QueriesOperation:
  inline def apply[F[_], G[_]](using self: QueriesOperation[F, G]): QueriesOperation[F, G] = self

  given [F[_]] => InvariantK[[f[_]] =>> QueriesOperation[f, F]]:
    extension [G[_]](self: QueriesOperation[G, F])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): QueriesOperation[H, F] =
        self.mapK(fK)

  trait Read[F[_], G[_]] extends QueriesOperation[F, G]:
    self =>

    override def mapK[H[_]](fK: [A] => F[A] => H[A]): QueriesOperation.Read[H, G] = new Read[H, G]:
      override def empty: H[Unit] = fK(self.empty)

      override def lift[A](query: Reference[G, A]): H[A] = fK(self.lift(query))

  object Read:
    inline def apply[F[_], G[_]](using self: QueriesOperation.Read[F, G]): QueriesOperation.Read[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> QueriesOperation.Read[f, F]]:
      extension [G[_]](self: QueriesOperation.Read[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): QueriesOperation.Read[H, F] =
          self.mapK(fK)

  trait Write[F[_], G[_]] extends QueriesOperation[F, G]:
    self =>

    override def mapK[H[_]](fK: [A] => F[A] => H[A]): QueriesOperation.Write[H, G] = new Write[H, G]:
      override def empty: H[Unit] = fK(self.empty)

      override def lift[A](query: Reference[G, A]): H[A] = fK(self.lift(query))

  object Write:
    inline def apply[F[_], G[_]](using self: QueriesOperation.Write[F, G]): QueriesOperation.Write[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> QueriesOperation.Write[f, F]]:
      extension [G[_]](self: QueriesOperation.Write[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): Write[H, F] = self.mapK(fK)
