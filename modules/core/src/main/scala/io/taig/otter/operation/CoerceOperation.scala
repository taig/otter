package io.taig.otter.operation

import io.taig.otter.Reference
import io.taig.otter.InvariantK

trait CoerceOperation[F[_], G[_]]:
  self =>

  def lift[A](schema: Reference[G, A]): F[A]

  extension [A](self: F[A]) def schema: Reference[G, ?]

  def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): CoerceOperation[H, G] = new CoerceOperation[H, G]:
    override def lift[A](schema: Reference[G, A]): H[A] = fK(self.lift(schema))

    extension [A](ha: H[A]) override def schema: Reference[G, ?] = self.schema(gK(ha))

object CoerceOperation:
  trait Read[F[_], G[_]] extends CoerceOperation[F, G]:
    self =>

    override def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): CoerceOperation.Read[H, G] =
      new Read[H, G]:
        override def lift[A](schema: Reference[G, A]): H[A] = fK(self.lift(schema))

        extension [A](ha: H[A]) override def schema: Reference[G, ?] = self.schema(gK(ha))

  object Read:
    inline def apply[F[_], G[_]](using self: CoerceOperation.Read[F, G]): CoerceOperation.Read[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> CoerceOperation.Read[f, F]]:
      extension [G[_]](fa: Read[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): CoerceOperation.Read[H, F] =
          fa.imapK(fK)(gK)

  trait Write[F[_], G[_]] extends CoerceOperation[F, G]:
    self =>

    override def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): CoerceOperation.Write[H, G] =
      new Write[H, G]:
        override def lift[A](schema: Reference[G, A]): H[A] = fK(self.lift(schema))

        extension [A](ha: H[A]) override def schema: Reference[G, ?] = self.schema(gK(ha))

  object Write:
    inline def apply[F[_], G[_]](using self: CoerceOperation.Write[F, G]): CoerceOperation.Write[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> CoerceOperation.Write[f, F]]:
      extension [G[_]](fa: CoerceOperation.Write[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): CoerceOperation.Write[H, F] =
          fa.imapK(fK)(gK)

  inline def apply[F[_], G[_]](using self: CoerceOperation[F, G]): CoerceOperation[F, G] = self

  given [F[_]] => InvariantK[[f[_]] =>> CoerceOperation[f, F]]:
    extension [G[_]](fa: CoerceOperation[G, F])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): CoerceOperation[H, F] =
        fa.imapK(fK)(gK)
