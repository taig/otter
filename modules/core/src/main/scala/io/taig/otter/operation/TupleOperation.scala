package io.taig.otter.operation

import io.taig.otter.Reference
import io.taig.otter.Append
import cats.InvariantSemigroupal

trait TupleOperation[Self[_[a] <: Bound[a], _], Bound[_]]:
  self =>

  def empty: Self[Nothing, Unit]

  def lift[S[a] <: Bound[a], A](schema: Reference[S, A]): Self[S, A]

  extension [S[a] >: T[a] <: Bound[a], T[a] <: Bound[a], A](self: Self[S, A])
    final def :*[B](schema: => T[B])(using InvariantSemigroupal[Self[S, *]]): Self[S, Append[A, B]] =
      Append(self, lift(schema = Reference.later(schema)))

  extension [
      Self1[s[a] <: Bound1[a], a],
      Bound1[a] >: Bound[a],
      S[a] <: Bound[a],
      T[a] >: S[a] <: Bound1[a],
      A
  ](self: Self[S, A])
    final def :*[B](schema: => T[B])(using
        TupleOperation[Self1, Bound1]
    )(using InvariantSemigroupal[Self1[T, *]])(using evidence: Self[S, A] <:< Self1[T, A]): Self1[T, Append[A, B]] =
      Append(evidence(self), TupleOperation[Self1, Bound1].lift(schema = Reference.later(schema)))

  def mapK[F[_[a] <: Bound[a], _]](fK: [S[a] <: Bound[a], A] => Self[S, A] => F[S, A]): TupleOperation[F, Bound] =
    new TupleOperation[F, Bound]:
      override def empty: F[Nothing, Unit] = fK(self.empty)

      override def lift[S[a] <: Bound[a], A](schema: Reference[S, A]): F[S, A] = fK(self.lift(schema))

object TupleOperation:
  trait Read[Self[_[a] <: Bound[a], _], Bound[_]] extends TupleOperation[Self, Bound]:
    self =>

    final override def mapK[F[_[a] <: Bound[a], _]](
        fK: [S[a] <: Bound[a], A] => Self[S, A] => F[S, A]
    ): TupleOperation.Read[F, Bound] =
      new Read[F, Bound]:
        override def empty: F[Nothing, Unit] = fK(self.empty)

        override def lift[S[a] <: Bound[a], A](schema: Reference[S, A]): F[S, A] = fK(self.lift(schema))

  object Read:
    inline def apply[Self[_[a] <: Bound[a], _], Bound[_]](using
        self: TupleOperation.Read[Self, Bound]
    ): TupleOperation.Read[Self, Bound] = self

  trait Write[Self[_[a] <: Bound[a], _], Bound[_]] extends TupleOperation[Self, Bound]:
    self =>

    final override def mapK[F[_[a] <: Bound[a], _]](
        fK: [S[a] <: Bound[a], A] => Self[S, A] => F[S, A]
    ): TupleOperation.Write[F, Bound] =
      new Write[F, Bound]:
        override def empty: F[Nothing, Unit] = fK(self.empty)

        override def lift[S[a] <: Bound[a], A](schema: Reference[S, A]): F[S, A] = fK(self.lift(schema))

  object Write:
    inline def apply[Self[_[a] <: Bound[a], _], Bound[_]](using
        self: TupleOperation.Write[Self, Bound]
    ): TupleOperation.Write[Self, Bound] = self

  inline def apply[Self[_[a] <: Bound[a], _], Bound[_]](using
      self: TupleOperation[Self, Bound]
  ): TupleOperation[Self, Bound] = self
