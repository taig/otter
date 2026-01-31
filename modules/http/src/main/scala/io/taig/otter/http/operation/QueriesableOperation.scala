package io.taig.otter.http.operation

import io.taig.otter.Reference
import cats.InvariantSemigroupal
import io.taig.otter.Append

trait QueriesableOperation[F[_], G[_]]:
  self =>

  extension [A](fa: F[A]) def toQueries: G[A]

  extension [F1[a] >: F[a], G1[a] >: G[a] <: Matchable, A](fa: F[A])
    final inline def :*[B](
        query: => F1[B]
    )(using QueriesOperation[G1, F1], InvariantSemigroupal[G1]): G1[Append[A, B]] =
      Append(fa.toQueries, QueriesOperation[G1, F1].lift(query = Reference.later(query)))

object QueriesableOperation:
  trait Read[F[_], G[_]] extends QueriesableOperation[F, G]

  object Read:
    inline def apply[F[_], G[_]](using self: QueriesableOperation.Read[F, G]): QueriesableOperation.Read[F, G] = self

    def derived[F[_], G[_]](using
        QueriesOperation.Read[G, F]
    ): QueriesableOperation.Read[F, G] = new QueriesableOperation.Read[F, G]:
      extension [A](fa: F[A]) override def toQueries: G[A] = QueriesOperation[G, F].lift(query = Reference.now(fa))

  trait Write[F[_], G[_]] extends QueriesableOperation[F, G]

  object Write:
    inline def apply[F[_], G[_]](using self: QueriesableOperation.Write[F, G]): QueriesableOperation.Write[F, G] = self

    def derived[F[_], G[_]](using
        QueriesOperation.Write[G, F]
    ): QueriesableOperation.Write[F, G] = new QueriesableOperation.Write[F, G]:
      extension [A](fa: F[A]) override def toQueries: G[A] = QueriesOperation[G, F].lift(query = Reference.now(fa))

  inline def apply[F[_], G[_]](using self: QueriesableOperation[F, G]): QueriesableOperation[F, G] = self

  def derived[F[_], G[_]](using
      QueriesOperation[G, F]
  ): QueriesableOperation[F, G] = new QueriesableOperation[F, G]:
    extension [A](fa: F[A]) override def toQueries: G[A] = QueriesOperation[G, F].lift(query = Reference.now(fa))
