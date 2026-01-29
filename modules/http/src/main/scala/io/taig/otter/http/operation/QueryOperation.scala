package io.taig.otter.http.operation

import io.taig.otter.Reference
import io.taig.otter.InvariantK

trait QueryOperation[F[_], G[_]]:
  self =>

  def lift[A](name: String, parameter: Reference[G, A]): F[A]

  def mapK[H[_]](fK: [A] => F[A] => H[A]): QueryOperation[H, G] = new QueryOperation[H, G]:
    override def lift[A](name: String, parameter: Reference[G, A]): H[A] =
      fK(self.lift(name, parameter))

object QueryOperation:
  trait Read[F[_], G[_]] extends QueryOperation[F, G]:
    self =>

    override def mapK[H[_]](fK: [A] => F[A] => H[A]): QueryOperation.Read[H, G] = new Read[H, G]:
      override def lift[A](name: String, parameter: Reference[G, A]): H[A] =
        fK(self.lift(name, parameter))

  object Read:
    inline def apply[F[_], G[_]](using self: QueryOperation.Read[F, G]): QueryOperation.Read[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> QueryOperation.Read[f, F]]:
      extension [G[_]](self: QueryOperation.Read[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): Read[H, F] = self.mapK(fK)

  trait Write[F[_], G[_]] extends QueryOperation[F, G]:
    self =>

    override def mapK[H[_]](fK: [A] => F[A] => H[A]): QueryOperation.Write[H, G] = new Write[H, G]:
      override def lift[A](name: String, parameter: Reference[G, A]): H[A] =
        fK(self.lift(name, parameter))

  object Write:
    inline def apply[F[_], G[_]](using self: QueryOperation.Write[F, G]): QueryOperation.Write[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> QueryOperation.Write[f, F]]:
      extension [G[_]](self: QueryOperation.Write[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): Write[H, F] = self.mapK(fK)

  inline def apply[F[_], G[_]](using self: QueryOperation[F, G]): QueryOperation[F, G] = self

  given [F[_]] => InvariantK[[f[_]] =>> QueryOperation[f, F]]:
    extension [G[_]](self: QueryOperation[G, F])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): QueryOperation[H, F] = self.mapK(fK)
