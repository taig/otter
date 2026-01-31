package io.taig.otter.operation

import cats.InvariantSemigroupal
import io.taig.otter.Append
import io.taig.otter.Reference

trait TupleableOperation[F[_], G[_]]:
  extension [A](fa: F[A]) def toTuple: G[A]

  extension [F1[a] >: F[a], G1[a] >: G[a] <: Matchable, A](fa: F[A])
    final inline def :*[B](
        schema: => F1[B]
    )(using TupleOperation[G1, F1], InvariantSemigroupal[G1]): G1[Append[A, B]] =
      Append(fa.toTuple, TupleOperation[G1, F1].lift(schema = Reference.later(schema)))

object TupleableOperation:
  trait Read[F[_], G[_]] extends TupleableOperation[F, G]

  object Read:
    inline def apply[F[_], G[_]](using self: TupleableOperation.Read[F, G]): TupleableOperation.Read[F, G] = self

    def derived[F[_], G[_]](using
        TupleOperation[G, F]
    ): TupleableOperation.Read[F, G] = new TupleableOperation.Read[F, G]:
      extension [A](fa: F[A]) override def toTuple: G[A] = TupleOperation[G, F].lift(schema = Reference.now(fa))

  trait Write[F[_], G[_]] extends TupleableOperation[F, G]

  object Write:
    inline def apply[F[_], G[_]](using self: TupleableOperation.Write[F, G]): TupleableOperation.Write[F, G] = self

    def derived[F[_], G[_]](using
        TupleOperation[G, F]
    ): TupleableOperation.Write[F, G] = new TupleableOperation.Write[F, G]:
      extension [A](fa: F[A]) override def toTuple: G[A] = TupleOperation[G, F].lift(schema = Reference.now(fa))

  inline def apply[F[_], G[_]](using self: TupleableOperation[F, G]): TupleableOperation[F, G] = self

  def derived[F[_], G[_]](using
      TupleOperation[G, F]
  ): TupleableOperation[F, G] = new TupleableOperation[F, G]:
    extension [A](fa: F[A]) override def toTuple: G[A] = TupleOperation[G, F].lift(schema = Reference.now(fa))
