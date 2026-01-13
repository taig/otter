package io.taig.otter.operation

import cats.InvariantSemigroupal
import cats.data.Chain
import io.taig.otter.Append
import io.taig.otter.InvariantK
import io.taig.otter.Reference

trait TupleOperation[F[_], G[_]]:
  self =>

  def empty: F[Unit]

  def lift[A](schema: Reference[G, A]): F[A]

  extension [A](fa: F[A])
    def optional: F[Option[A]]

    def schemas: Chain[Reference[G, ?]]

    final def size: Long = schemas.length

  extension [F1[a] >: F[a] <: Matchable, G1[a] >: G[a], A](fa: F[A])
    final def :*[B](schema: => G1[B])(using TupleOperation[F1, G1], InvariantSemigroupal[F1]): F1[Append[A, B]] =
      Append(fa, TupleOperation[F1, G1].lift(schema = Reference.later(schema)))

  def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): TupleOperation[H, G] =
    new TupleOperation[H, G]:
      override def empty: H[Unit] = fK(self.empty)

      override def lift[A](schema: Reference[G, A]): H[A] = fK(self.lift(schema))

      extension [A](ha: H[A])
        override def optional: H[Option[A]] = fK(self.optional(gK(ha)))

        override def schemas: Chain[Reference[G, ?]] = self.schemas(gK(ha))

object TupleOperation:
  trait Read[F[_], G[_]] extends TupleOperation[F, G]:
    self =>

    final override def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): TupleOperation.Read[H, G] =
      new TupleOperation.Read[H, G]:
        override def empty: H[Unit] = fK(self.empty)

        override def lift[A](schema: Reference[G, A]): H[A] = fK(self.lift(schema))
        extension [A](ha: H[A])
          override def optional: H[Option[A]] = fK(self.optional(gK(ha)))

          override def schemas: Chain[Reference[G, ?]] = self.schemas(gK(ha))

  object Read:
    inline def apply[F[_], G[_]](using self: TupleOperation.Read[F, G]): TupleOperation.Read[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> TupleOperation.Read[f, F]]:
      extension [G[_]](fa: TupleOperation.Read[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): TupleOperation.Read[H, F] =
          fa.imapK(fK)(gK)

  trait Write[F[_], G[_]] extends TupleOperation[F, G]:
    self =>

    final override def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): TupleOperation.Write[H, G] =
      new TupleOperation.Write[H, G]:
        override def empty: H[Unit] = fK(self.empty)

        override def lift[A](schema: Reference[G, A]): H[A] = fK(self.lift(schema))

        extension [A](ha: H[A])
          override def optional: H[Option[A]] = fK(self.optional(gK(ha)))

          override def schemas: Chain[Reference[G, ?]] = self.schemas(gK(ha))

  object Write:
    inline def apply[F[_], G[_]](using self: TupleOperation.Write[F, G]): TupleOperation.Write[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> TupleOperation.Write[f, F]]:
      extension [G[_]](fa: TupleOperation.Write[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): TupleOperation.Write[H, F] =
          fa.imapK(fK)(gK)

  inline def apply[F[_], G[_]](using self: TupleOperation[F, G]): TupleOperation[F, G] = self

  given [F[_]] => InvariantK[[f[_]] =>> TupleOperation[f, F]]:
    extension [G[_]](fa: TupleOperation[G, F])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): TupleOperation[H, F] =
        fa.imapK(fK)(gK)
