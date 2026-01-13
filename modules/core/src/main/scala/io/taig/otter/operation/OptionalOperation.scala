package io.taig.otter.operation

import io.taig.otter.InvariantK
import io.taig.otter.Reference

trait OptionalOperation[F[_], G[_]]:
  self =>

  def lift[A](schema: => Reference[G, A]): F[Option[A]]

  def lift[A](schema: => Reference[G, A], default: => A): F[A]

  extension [A](fa: F[A]) def schema: Reference[G, ?]

  def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): OptionalOperation[H, G] =
    new OptionalOperation[H, G]:
      override def lift[A](schema: => Reference[G, A]): H[Option[A]] = fK(self.lift(schema))

      override def lift[A](schema: => Reference[G, A], default: => A): H[A] = fK(self.lift(schema, default))

      extension [A](fa: H[A]) override def schema: Reference[G, ?] = self.schema(gK(fa))

object OptionalOperation:
  trait Read[F[_], G[_]] extends OptionalOperation[F, G]:
    self =>

    override def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): OptionalOperation.Read[H, G] =
      new Read[H, G]:
        override def lift[A](schema: => Reference[G, A]): H[Option[A]] = fK(self.lift(schema))

        override def lift[A](schema: => Reference[G, A], default: => A): H[A] = fK(self.lift(schema, default))

        extension [A](fa: H[A]) override def schema: Reference[G, ?] = self.schema(gK(fa))

  object Read:
    inline def apply[F[_], G[_]](using self: OptionalOperation.Read[F, G]): OptionalOperation.Read[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> OptionalOperation.Read[f, F]]:
      extension [G[_]](fa: OptionalOperation.Read[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): OptionalOperation.Read[H, F] =
          fa.imapK(fK)(gK)

  trait Write[F[_], G[_]] extends OptionalOperation[F, G]:
    self =>

    override def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): OptionalOperation.Write[H, G] =
      new Write[H, G]:
        override def lift[A](schema: => Reference[G, A]): H[Option[A]] = fK(self.lift(schema))

        override def lift[A](schema: => Reference[G, A], default: => A): H[A] = fK(self.lift(schema, default))

        extension [A](fa: H[A]) override def schema: Reference[G, ?] = self.schema(gK(fa))

  object Write:
    inline def apply[F[_], G[_]](using self: OptionalOperation.Write[F, G]): OptionalOperation.Write[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> OptionalOperation.Write[f, F]]:
      extension [G[_]](fa: OptionalOperation.Write[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): OptionalOperation.Write[H, F] =
          fa.imapK(fK)(gK)

  inline def apply[F[_], G[_]](using self: OptionalOperation[F, G]): OptionalOperation[F, G] = self

  given [F[_]] => InvariantK[[f[_]] =>> OptionalOperation[f, F]]:
    extension [G[_]](fa: OptionalOperation[G, F])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): OptionalOperation[H, F] =
        fa.imapK(fK)(gK)
