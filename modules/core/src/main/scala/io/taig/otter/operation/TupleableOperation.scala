package io.taig.otter.operation

import io.taig.otter.Reference
import io.taig.otter.Append
import cats.InvariantSemigroupal
import io.taig.otter.InvariantK

trait TupleableOperation[F[_], G[_]]:
  self =>

  extension [A](ga: G[A]) def toTuple: F[A]

  extension [F1[a] >: F[a] <: Matchable, G1[a] >: G[a], A](ga: G[A])
    final def :*[B](
        schema: => G1[B]
    )(using TupleOperation[F1, G1], InvariantSemigroupal[F1]): F1[Append[A, B]] =
      Append(ga.toTuple, TupleOperation[F1, G1].lift(schema = Reference.later(schema)))

  def mapK[H[_]](fK: [A] => F[A] => H[A]): TupleableOperation[H, G] = new TupleableOperation[H, G]:
    extension [A](ga: G[A]) override def toTuple: H[A] = fK(self.toTuple(ga))

object TupleableOperation:
  trait Read[F[_], G[_]] extends TupleableOperation[F, G]:
    self =>

    final override def mapK[H[_]](fK: [A] => F[A] => H[A]): TupleableOperation.Read[H, G] =
      new TupleableOperation.Read[H, G]:
        extension [A](ga: G[A]) override def toTuple: H[A] = fK(self.toTuple(ga))

  object Read:
    inline def apply[F[_], G[_]](using self: TupleableOperation.Read[F, G]): TupleableOperation.Read[F, G] = self

    def derived[F[_], G[_]](using
        TupleOperation[F, G]
    ): TupleableOperation.Read[F, G] = new TupleableOperation.Read[F, G]:
      extension [A](self: G[A]) override def toTuple: F[A] = TupleOperation[F, G].lift(schema = Reference.now(self))

    given [F[_]] => InvariantK[[f[_]] =>> TupleableOperation.Read[f, F]]:
      extension [G[_]](fa: TupleableOperation.Read[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): TupleableOperation.Read[H, F] =
          fa.mapK(fK)

  trait Write[F[_], G[_]] extends TupleableOperation[F, G]:
    self =>

    final override def mapK[H[_]](fK: [A] => F[A] => H[A]): TupleableOperation.Write[H, G] =
      new TupleableOperation.Write[H, G]:
        extension [A](ga: G[A]) override def toTuple: H[A] = fK(self.toTuple(ga))

  object Write:
    inline def apply[F[_], G[_]](using self: TupleableOperation.Write[F, G]): TupleableOperation.Write[F, G] = self

    def derived[F[_], G[_]](using
        TupleOperation[F, G]
    ): TupleableOperation.Write[F, G] = new TupleableOperation.Write[F, G]:
      extension [A](self: G[A]) override def toTuple: F[A] = TupleOperation[F, G].lift(schema = Reference.now(self))

    given [F[_]] => InvariantK[[f[_]] =>> TupleableOperation.Write[f, F]]:
      extension [G[_]](fa: TupleableOperation.Write[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): TupleableOperation.Write[H, F] =
          fa.mapK(fK)

  inline def apply[F[_], G[_]](using self: TupleableOperation[F, G]): TupleableOperation[F, G] = self

  def derived[F[_], G[_]](using
      TupleOperation[F, G]
  ): TupleableOperation[F, G] = new TupleableOperation[F, G]:
    extension [A](self: G[A]) override def toTuple: F[A] = TupleOperation[F, G].lift(schema = Reference.now(self))

  given [F[_]] => InvariantK[[f[_]] =>> TupleableOperation[f, F]]:
    extension [G[_]](fa: TupleableOperation[G, F])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): TupleableOperation[H, F] = fa.mapK(fK)
