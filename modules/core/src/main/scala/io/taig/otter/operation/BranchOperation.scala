package io.taig.otter.operation

import io.taig.otter.InvariantK
import io.taig.otter.Reference

trait BranchOperation[F[_], G[_]]:
  self =>

  def lift[A](name: String, schema: Reference[G, A]): F[A]

  extension [A](fa: F[A]) def schema: Reference[G, ?]

  def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): BranchOperation[H, G] =
    new BranchOperation[H, G]:
      override def lift[A](name: String, schema: Reference[G, A]): H[A] = fK(self.lift(name, schema))

      extension [A](ha: H[A]) override def schema: Reference[G, ?] = self.schema(gK(ha))

object BranchOperation:
  trait Read[F[_], G[_]] extends BranchOperation[F, G]:
    self =>

    final override def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): BranchOperation.Read[H, G] =
      new Read[H, G]:
        override def lift[A](name: String, schema: Reference[G, A]): H[A] = fK(self.lift(name, schema))

        extension [A](ha: H[A]) override def schema: Reference[G, ?] = self.schema(gK(ha))

  object Read:
    inline def apply[F[_], G[_]](using self: BranchOperation.Read[F, G]): BranchOperation.Read[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> BranchOperation.Read[f, F]]:
      extension [G[_]](fa: BranchOperation.Read[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): BranchOperation.Read[H, F] =
          fa.imapK(fK)(gK)

  trait Write[F[_], G[_]] extends BranchOperation[F, G]:
    self =>

    final override def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): BranchOperation.Write[H, G] =
      new Write[H, G]:
        override def lift[A](name: String, schema: Reference[G, A]): H[A] = fK(self.lift(name, schema))

        extension [A](ha: H[A]) override def schema: Reference[G, ?] = self.schema(gK(ha))

  object Write:
    inline def apply[F[_], G[_]](using self: BranchOperation.Write[F, G]): BranchOperation.Write[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> BranchOperation.Write[f, F]]:
      extension [G[_]](fa: BranchOperation.Write[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): BranchOperation.Write[H, F] =
          fa.imapK(fK)(gK)

  inline def apply[F[_], G[_]](using self: BranchOperation[F, G]): BranchOperation[F, G] = self

  given [F[_]] => InvariantK[[f[_]] =>> BranchOperation[f, F]]:
    extension [G[_]](fa: BranchOperation[G, F])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): BranchOperation[H, F] =
        fa.imapK(fK)(gK)
