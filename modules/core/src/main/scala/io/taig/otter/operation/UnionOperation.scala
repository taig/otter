package io.taig.otter.operation

import cats.data.NonEmptyChain
import io.taig.otter.InvariantK
import io.taig.otter.Reference

trait UnionOperation[F[_], G[_]]:
  self =>

  def lift[A](branch: Reference[G, A]): F[A]

  extension [A](fa: F[A]) def branches: NonEmptyChain[Reference[G, ?]]

  extension [A](fa: F[A]) def orElse[B](schema: F[B]): F[Either[A, B]]

  extension [F1[a] >: F[a], A](fa: F[A])
    final def :+[B](branch: => G[B]): F1[Either[A, B]] = fa.orElse(lift(Reference.later(branch)))

  def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): UnionOperation[H, G] =
    new UnionOperation[H, G]:
      override def lift[A](branch: Reference[G, A]): H[A] = fK(self.lift(branch))

      extension [A](fa: H[A])
        override def orElse[B](schema: H[B]): H[Either[A, B]] = fK(self.orElse(gK(fa))(gK(schema)))

      extension [A](fa: H[A]) override def branches: NonEmptyChain[Reference[G, ?]] = self.branches(gK(fa))

object UnionOperation:
  trait Read[F[_], G[_]] extends UnionOperation[F, G]:
    self =>

    override def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): UnionOperation.Read[H, G] =
      new Read[H, G]:
        override def lift[A](branch: Reference[G, A]): H[A] = fK(self.lift(branch))

        extension [A](fa: H[A])
          override def orElse[B](schema: H[B]): H[Either[A, B]] = fK(self.orElse(gK(fa))(gK(schema)))

        extension [A](fa: H[A]) override def branches: NonEmptyChain[Reference[G, ?]] = self.branches(gK(fa))

  object Read:
    inline def apply[F[_], G[_]](using self: UnionOperation.Read[F, G]): UnionOperation.Read[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> UnionOperation.Read[f, F]]:
      extension [G[_]](fa: UnionOperation.Read[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): UnionOperation.Read[H, F] =
          fa.imapK(fK)(gK)

  trait Write[F[_], G[_]] extends UnionOperation[F, G]:
    self =>

    override def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): UnionOperation.Write[H, G] =
      new Write[H, G]:
        override def lift[A](branch: Reference[G, A]): H[A] = fK(self.lift(branch))

        extension [A](fa: H[A])
          override def orElse[B](schema: H[B]): H[Either[A, B]] =
            fK(self.orElse(gK(fa))(gK(schema)))

        extension [A](fa: H[A]) override def branches: NonEmptyChain[Reference[G, ?]] = self.branches(gK(fa))

  object Write:
    inline def apply[F[_], G[_]](using self: UnionOperation.Write[F, G]): UnionOperation.Write[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> UnionOperation.Write[f, F]]:
      extension [G[_]](fa: UnionOperation.Write[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): UnionOperation.Write[H, F] =
          fa.imapK(fK)(gK)

  inline def apply[F[_], G[_]](using self: UnionOperation[F, G]): UnionOperation[F, G] = self

  given [F[_]] => InvariantK[[f[_]] =>> UnionOperation[f, F]]:
    extension [G[_]](fa: UnionOperation[G, F])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): UnionOperation[H, F] =
        fa.imapK(fK)(gK)
