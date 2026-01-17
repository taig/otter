package io.taig.otter.operation

import cats.InvariantSemigroupal
import io.taig.otter.Append
import io.taig.otter.Reference

trait RecordableOperation[F[_], G[_]]:
  self =>

  extension [A](fa: F[A]) def toRecord: G[A]

  extension [F1[a] >: F[a], G1[a] >: G[a] <: Matchable, A](fa: F[A])
    final inline def :*[B](
        field: => F1[B]
    )(using RecordOperation[G1, F1], InvariantSemigroupal[G1]): G1[Append[A, B]] =
      Append(fa.toRecord, RecordOperation[G1, F1].lift(field = Reference.later(field)))

object RecordableOperation:
  trait Read[F[_], G[_]] extends RecordableOperation[F, G]

  object Read:
    inline def apply[F[_], G[_]](using self: RecordableOperation.Read[F, G]): RecordableOperation.Read[F, G] = self

    def derived[F[_], G[_]](using RecordOperation.Read[G, F]): RecordableOperation.Read[F, G] =
      new RecordableOperation.Read[F, G]:
        extension [A](fa: F[A]) override def toRecord: G[A] = RecordOperation.Read[G, F].lift(field = Reference.now(fa))

  trait Write[F[_], G[_]] extends RecordableOperation[F, G]

  object Write:
    inline def apply[F[_], G[_]](using self: RecordableOperation.Write[F, G]): RecordableOperation.Write[F, G] = self

    def derived[F[_], G[_]](using RecordOperation.Write[G, F]): RecordableOperation.Write[F, G] =
      new RecordableOperation.Write[F, G]:
        extension [A](fa: F[A])
          override def toRecord: G[A] = RecordOperation.Write[G, F].lift(field = Reference.now(fa))

  inline def apply[F[_], G[_]](using self: RecordableOperation[F, G]): RecordableOperation[F, G] = self

  def derived[F[_], G[_]](using RecordOperation[G, F]): RecordableOperation[F, G] =
    new RecordableOperation[F, G]:
      extension [A](fa: F[A]) override def toRecord: G[A] = RecordOperation[G, F].lift(field = Reference.now(fa))
