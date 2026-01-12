package io.taig.otter.operation

import cats.InvariantSemigroupal
import cats.data.Chain
import io.taig.otter.Append
import io.taig.otter.InvariantK
import io.taig.otter.Reference

trait RecordOperation[F[_], G[_]]:
  self =>

  def empty: F[Unit]

  def lift[A](field: Reference[G, A]): F[A]

  extension [A](fa: F[A]) def fields: Chain[Reference[G, ?]]

  extension [F1[a] >: F[a] <: Matchable, G1[a] >: G[a], A](fa: F[A])
    final def :*[B](field: => G1[B])(using RecordOperation[F1, G1], InvariantSemigroupal[F1]): F1[Append[A, B]] =
      Append(fa, RecordOperation[F1, G1].lift(field = Reference.later(field)))

  def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): RecordOperation[H, G] =
    new RecordOperation[H, G]:
      override def empty: H[Unit] = fK(self.empty)

      override def lift[A](field: Reference[G, A]): H[A] = fK(self.lift(field))

      extension [A](fa: H[A]) override def fields: Chain[Reference[G, ?]] = self.fields(gK(fa))

object RecordOperation:
  trait Read[F[_], G[_]] extends RecordOperation[F, G]:
    self =>

    override def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): RecordOperation.Read[H, G] =
      new Read[H, G]:
        override def empty: H[Unit] = fK(self.empty)

        override def lift[A](field: Reference[G, A]): H[A] = fK(self.lift(field))

        extension [A](fa: H[A]) override def fields: Chain[Reference[G, ?]] = self.fields(gK(fa))

  object Read:
    inline def apply[F[_], G[_]](using self: RecordOperation.Read[F, G]): RecordOperation.Read[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> RecordOperation.Read[f, F]]:
      extension [G[_]](fa: RecordOperation.Read[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): RecordOperation.Read[H, F] =
          fa.imapK(fK)(gK)

  trait Write[F[_], G[_]] extends RecordOperation[F, G]:
    self =>

    override def imapK[H[_]](fK: [A] => F[A] => H[A])(gK: [A] => H[A] => F[A]): RecordOperation.Write[H, G] =
      new Write[H, G]:
        override def empty: H[Unit] = fK(self.empty)

        override def lift[A](field: Reference[G, A]): H[A] = fK(self.lift(field))

        extension [A](fa: H[A]) override def fields: Chain[Reference[G, ?]] = self.fields(gK(fa))

  object Write:
    inline def apply[F[_], G[_]](using self: RecordOperation.Write[F, G]): RecordOperation.Write[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> RecordOperation.Write[f, F]]:
      extension [G[_]](fa: RecordOperation.Write[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): RecordOperation.Write[H, F] =
          fa.imapK(fK)(gK)

  inline def apply[F[_], G[_]](using self: RecordOperation[F, G]): RecordOperation[F, G] = self

  given [F[_]] => InvariantK[[f[_]] =>> RecordOperation[f, F]]:
    extension [G[_]](fa: RecordOperation[G, F])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): RecordOperation[H, F] =
        fa.imapK(fK)(gK)
