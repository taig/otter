package io.taig.otter.operation

import scala.annotation.targetName
import io.taig.otter.Append
import cats.Invariant
import cats.syntax.all.*

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

  extension [S[a] <: Bound[a], A](self: Self[S, A])
    def zip[T[a] >: S[a] <: Bound[a], B](schema: Self[T, B]): Self[T, (A, B)]

    final def :*[T[a] >: S[a] <: Bound[a], B](schema: T[B])(using
        append: Append[A, B]
    )(using Invariant[Self[T, *]]): Self[T, append.Out] =
      self.zip(apply(schema)).imap(append.apply)(append.unapply)

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
