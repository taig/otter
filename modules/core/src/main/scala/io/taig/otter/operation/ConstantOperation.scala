package io.taig.otter.operation

import io.taig.otter.Reference
import cats.Eq
import io.taig.otter.InvariantK
import cats.Eval
import io.taig.data.Data

trait ConstantOperation[F[_], G[_]]:
  self =>

  def lift[A](schema: Reference[G, A], value: Eval[A], eq: Eq[A]): F[A]

  extension [A](fa: F[A])
    def schema: Reference[G, ?]

    def value: Eval[Data]

  def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): ConstantOperation[H, G] =
    new ConstantOperation[H, G]:
      override def lift[A](schema: Reference[G, A], value: Eval[A], eq: Eq[A]): H[A] = fK(self.lift(schema, value, eq))

      extension [A](ha: H[A])
        override def schema: Reference[G, ?] = self.schema(gK(ha))

        override def value: Eval[Data] = self.value(gK(ha))

object ConstantOperation:
  trait Read[F[_], G[_]] extends ConstantOperation[F, G]:
    self =>

    final override def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): ConstantOperation.Read[H, G] =
      new Read[H, G]:
        override def lift[A](schema: Reference[G, A], value: Eval[A], eq: Eq[A]): H[A] =
          fK(self.lift(schema, value, eq))

        extension [A](ha: H[A])
          override def schema: Reference[G, ?] = self.schema(gK(ha))

          override def value: Eval[Data] = self.value(gK(ha))

  object Read:
    inline def apply[F[_], G[_]](using self: ConstantOperation.Read[F, G]): ConstantOperation.Read[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> ConstantOperation.Read[f, F]]:
      extension [G[_]](fa: ConstantOperation.Read[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): ConstantOperation.Read[H, F] =
          fa.imapK(fK)(gK)

  trait Write[F[_], G[_]] extends ConstantOperation[F, G]:
    self =>

    def lift[A](schema: Reference[G, A], value: Eval[A]): F[A]

    final override def lift[A](schema: Reference[G, A], value: Eval[A], eq: Eq[A]): F[A] = lift(schema, value)

    final override def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): ConstantOperation.Write[H, G] =
      new Write[H, G]:
        override def lift[A](schema: Reference[G, A], value: Eval[A]): H[A] = fK(self.lift(schema, value))

        extension [A](ha: H[A])
          override def schema: Reference[G, ?] = self.schema(gK(ha))

          override def value: Eval[Data] = self.value(gK(ha))

  object Write:
    inline def apply[F[_], G[_]](using self: ConstantOperation.Write[F, G]): ConstantOperation.Write[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> ConstantOperation.Write[f, F]]:
      extension [G[_]](fa: ConstantOperation.Write[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): ConstantOperation.Write[H, F] =
          fa.imapK(fK)(gK)

  inline def apply[F[_], G[_]](using self: ConstantOperation[F, G]): ConstantOperation[F, G] = self

  given [F[_]] => InvariantK[[f[_]] =>> ConstantOperation[f, F]]:
    extension [G[_]](fa: ConstantOperation[G, F])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): ConstantOperation[H, F] =
        fa.imapK(fK)(gK)
