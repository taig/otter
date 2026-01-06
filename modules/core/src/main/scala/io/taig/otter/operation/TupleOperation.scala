package io.taig.otter.operation

import scala.annotation.targetName
import io.taig.otter.Append
import cats.Invariant
import cats.syntax.all.*
import io.taig.otter.Prepend

abstract class TupleOperation[
    Self[+s[a] <: Bound[a], a] <: SelfRead[s, a] & SelfWrite[s, a],
    SelfRead[+_[a] <: BoundRead[a], _],
    SelfWrite[+_[a] <: BoundWrite[a], _],
    Bound[a] <: BoundRead[a] & BoundWrite[a],
    BoundRead[_],
    BoundWrite[_]
] extends Operation[Self, SelfRead, SelfWrite, Bound, BoundRead, BoundWrite],
      TupleOperation.Read[SelfRead, BoundRead],
      TupleOperation.Write[SelfWrite, BoundWrite]:
  def apply[S[a] <: Bound[a], A](schema: => S[A]): Self[S, A]

  override def empty: Self[Nothing, Unit]

  extension [S[a] <: Bound[a], T[a] >: S[a] <: Bound[a], A](self: Self[S, A])
    def zip[B](schema: Self[T, B]): Self[T, (A, B)]

    final def :*[B](schema: T[B])(using append: Append[A, B])(using Invariant[Self[T, *]]): Self[T, append.Out] =
      self.zip(apply(schema)).imap(append.apply)(append.unapply)

  extension [S[a] >: T[a] <: Bound[a], T[a] <: Bound[a], A](self: S[A])
    final def *:[B](schema: Self[T, B])(using
        prepend: Prepend[A, B]
    )(using Invariant[Self[S, *]]): Self[S, prepend.Out] =
      apply(self).zip(schema).imap(prepend.apply)(prepend.unapply)

object TupleOperation:
  trait Read[Self[+_[a] <: Bound[a], _], Bound[_]] extends Operation.Read[Self, Bound]:
    @targetName("applyRead")
    def apply[S[a] <: Bound[a], A](schema: => S[A]): Self[S, A]

    def empty: Self[Nothing, Unit]

    extension [S[a] <: Bound[a], A](self: Self[S, A])
      @targetName("zipRead")
      def zip[T[a] >: S[a] <: Bound[a], B](schema: Self[T, B]): Self[T, (A, B)]

  trait Write[Self[+_[a] <: Bound[a], _], Bound[_]] extends Operation.Write[Self, Bound]:
    @targetName("applyWrite")
    def apply[S[a] <: Bound[a], A](schema: => S[A]): Self[S, A]

    def empty: Self[Nothing, Unit]

    extension [S[a] <: Bound[a], A](self: Self[S, A])
      @targetName("zipWrite")
      def zip[T[a] >: S[a] <: Bound[a], B](schema: Self[T, B]): Self[T, (A, B)]
