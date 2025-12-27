package io.taig.otter.operation

import scala.annotation.targetName
import io.taig.otter.Append
import io.taig.otter.Prepend

abstract class TupleableOperation[
    Self[a] <: Bound[a] & SelfRead[a] & SelfWrite[a],
    SelfRead[a] <: BoundRead[a],
    SelfWrite[a] <: BoundWrite[a],
    Tuple[+_[a] <: Bound[a], _],
    TupleRead[+_[a] <: BoundRead[a], _],
    TupleWrite[+_[a] <: BoundWrite[a], _],
    Bound[_],
    BoundRead[_],
    BoundWrite[_]
] extends TupleableOperation.Read[SelfRead, TupleRead, BoundRead],
      TupleableOperation.Write[SelfWrite, TupleWrite, BoundWrite]:
  extension [A](self: Self[A])
    def toTuple: Tuple[Self, A]

    @targetName("append")
    def :*[S[a] >: Self[a] <: Bound[a], B](schema: => S[B])(using append: Append[A, B]): Tuple[S, append.Out]

    @targetName("prepend")
    def *:[S[a] >: Self[a] <: Bound[a], B](schema: => S[B])(using prepend: Prepend[A, B]): Tuple[S, prepend.Out]

    @targetName("appendWithRead")
    final def :*[S[a] >: SelfRead[a] <: BoundRead[a], B](schema: => S[B])(using
        append: Append[A, B]
    ): TupleRead[S, append.Out] = (self: SelfRead[A]) :* schema

    @targetName("appendWithWrite")
    final def :*[S[a] >: SelfWrite[a] <: BoundWrite[a], B](schema: => S[B])(using
        append: Append[A, B]
    ): TupleWrite[S, append.Out] = (self: SelfWrite[A]) :* schema

  extension [S[a] >: SelfRead[a] <: BoundRead[a], A](self: S[A])
    @targetName("prependRead")
    override def *:[B](schema: => SelfRead[B])(using prepend: Prepend[A, B]): TupleRead[S, prepend.Out]

  extension [S[a] >: SelfWrite[a] <: BoundWrite[a], A](self: S[A])
    @targetName("prependWrite")
    override def *:[B](schema: => SelfWrite[B])(using prepend: Prepend[A, B]): TupleWrite[S, prepend.Out]

object TupleableOperation:
  trait Read[Self[a] <: Bound[a], Tuple[+_[a] <: Bound[a], _], Bound[_]]:
    extension [A](self: Self[A])
      @targetName("toTupleRead")
      def toTuple: Tuple[Self, A]

      @targetName("appendRead")
      def :*[S[a] >: Self[a] <: Bound[a], B](schema: => S[B])(using append: Append[A, B]): Tuple[S, append.Out]

    extension [S[a] >: Self[a] <: Bound[a], A](self: S[A])
      @targetName("prependRead")
      def *:[B](schema: => Self[B])(using prepend: Prepend[A, B]): Tuple[S, prepend.Out]

  trait Write[Self[a] <: Bound[a], Tuple[+_[a] <: Bound[a], _], Bound[_]]:
    extension [A](self: Self[A])
      @targetName("toTupleWrite")
      def toTuple: Tuple[Self, A]

      @targetName("appendWrite")
      def :*[S[a] >: Self[a] <: Bound[a], B](schema: => S[B])(using append: Append[A, B]): Tuple[S, append.Out]

    extension [S[a] >: Self[a] <: Bound[a], A](self: S[A])
      @targetName("prependWrite")
      def *:[B](schema: => Self[B])(using prepend: Prepend[A, B]): Tuple[S, prepend.Out]

  def derived[
      Self[a] <: Bound[a] & SelfRead[a] & SelfWrite[a],
      SelfRead[a] <: BoundRead[a],
      SelfWrite[a] <: BoundWrite[a],
      Tuple[+s[a] <: Bound[a], a] <: TupleRead[s, a] & TupleWrite[s, a],
      TupleRead[+_[a] <: BoundRead[a], _],
      TupleWrite[+_[a] <: BoundWrite[a], _],
      Bound[a] <: BoundRead[a] & BoundWrite[a],
      BoundRead[_],
      BoundWrite[_]
  ](using TupleOperation[Tuple, TupleRead, TupleWrite, Bound, BoundRead, BoundWrite]): TupleableOperation[
    Self,
    SelfRead,
    SelfWrite,
    Tuple,
    TupleRead,
    TupleWrite,
    Bound,
    BoundRead,
    BoundWrite
  ] = ???
