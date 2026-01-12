package io.taig.otter.operation

import io.taig.otter.InvariantK
import io.taig.otter.Reference

trait FieldOperation[F[_], G[_]]:
  self =>

  def lift[A](name: String, schema: Reference[G, A]): F[A]

  extension [A](fa: F[A])
    def optional: F[Option[A]]

    def optional(default: => A): F[A]

    def schema: Reference[G, ?]

  def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): FieldOperation[H, G] =
    new FieldOperation[H, G]:
      override def lift[A](name: String, schema: Reference[G, A]): H[A] = fK(self.lift(name, schema))

      extension [A](ha: H[A])
        override def optional: H[Option[A]] = fK(self.optional(gK(ha)))

        override def optional(default: => A): H[A] = fK(self.optional(gK(ha))(default))

        override def schema: Reference[G, ?] = self.schema(gK(ha))

object FieldOperation:
  trait Read[F[_], G[_]] extends FieldOperation[F, G]:
    self =>

    final override def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): FieldOperation.Read[H, G] =
      new Read[H, G]:
        override def lift[A](name: String, schema: Reference[G, A]): H[A] = fK(self.lift(name, schema))

        extension [A](ha: H[A])
          override def optional: H[Option[A]] = fK(self.optional(gK(ha)))

          override def optional(default: => A): H[A] = fK(self.optional(gK(ha))(default))

          override def schema: Reference[G, ?] = self.schema(gK(ha))

  object Read:
    inline def apply[F[_], G[_]](using self: FieldOperation.Read[F, G]): FieldOperation.Read[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> FieldOperation.Read[f, F]]:
      extension [G[_]](fa: FieldOperation.Read[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): FieldOperation.Read[H, F] =
          fa.imapK(fK)(gK)

  trait Write[F[_], G[_]] extends FieldOperation[F, G]:
    self =>

    extension [A](fa: F[A]) final override def optional(default: => A): F[A] = fa

    final override def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): FieldOperation.Write[H, G] =
      new Write[H, G]:
        override def lift[A](name: String, schema: Reference[G, A]): H[A] = fK(self.lift(name, schema))

        extension [A](ha: H[A])
          override def optional: H[Option[A]] = fK(self.optional(gK(ha)))

          override def schema: Reference[G, ?] = self.schema(gK(ha))

  object Write:
    inline def apply[F[_], G[_]](using self: FieldOperation.Write[F, G]): FieldOperation.Write[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> FieldOperation.Write[f, F]]:
      extension [G[_]](fa: FieldOperation.Write[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): FieldOperation.Write[H, F] =
          fa.imapK(fK)(gK)

  inline def apply[F[_], G[_]](using self: FieldOperation[F, G]): FieldOperation[F, G] = self

  given [F[_]] => InvariantK[[f[_]] =>> FieldOperation[f, F]]:
    extension [G[_]](fa: FieldOperation[G, F])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): FieldOperation[H, F] =
        fa.imapK(fK)(gK)
