package io.taig.otter.http.operation

import io.taig.otter.http.Http
import io.taig.otter.InvariantK

trait QueriesOperation[F[_]]:
  self =>

  def empty: F[Unit]

  def lift[A](query: Http.Query[A]): F[A]

  def mapK[G[_]](fK: [A] => F[A] => G[A]): QueriesOperation[G] = new QueriesOperation[G]:
    override def empty: G[Unit] = fK(self.empty)

    override def lift[A](query: Http.Query[A]): G[A] = fK(self.lift(query))

object QueriesOperation:
  inline def apply[F[_]](using self: QueriesOperation[F]): QueriesOperation[F] = self

  given InvariantK[QueriesOperation]:
    extension [G[_]](self: QueriesOperation[G])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): QueriesOperation[H] = self.mapK(fK)

  trait Read[F[_]] extends QueriesOperation[F]:
    self =>

    def lift[A](query: Http.Query.Read[A]): F[A]

    final override def lift[A](query: Http.Query[A]): F[A] = lift(query: Http.Query.Read[A])

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): QueriesOperation.Read[G] = new Read[G]:
      override def empty: G[Unit] = fK(self.empty)

      override def lift[A](query: Http.Query.Read[A]): G[A] = fK(self.lift(query))

  object Read:
    inline def apply[F[_]](using self: QueriesOperation.Read[F]): QueriesOperation.Read[F] = self

    given InvariantK[QueriesOperation.Read]:
      extension [G[_]](self: QueriesOperation.Read[G])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): Read[H] = self.mapK(fK)

  trait Write[F[_]] extends QueriesOperation[F]:
    self =>

    def lift[A](query: Http.Query.Write[A]): F[A]

    final override def lift[A](query: Http.Query[A]): F[A] = lift(query: Http.Query.Write[A])

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): QueriesOperation.Write[G] = new Write[G]:
      override def empty: G[Unit] = fK(self.empty)

      override def lift[A](query: Http.Query.Write[A]): G[A] = fK(self.lift(query))

  object Write:
    inline def apply[F[_]](using self: QueriesOperation.Write[F]): QueriesOperation.Write[F] = self

    given InvariantK[QueriesOperation.Write]:
      extension [G[_]](self: QueriesOperation.Write[G])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): Write[H] = self.mapK(fK)
