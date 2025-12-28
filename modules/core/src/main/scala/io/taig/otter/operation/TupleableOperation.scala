package io.taig.otter.operation

import scala.annotation.targetName
import io.taig.otter.Append
import io.taig.otter.Prepend

abstract class TupleableOperation[
    Tuple[+_[a] <: Bound[a], _],
    TupleRead[+_[a] <: BoundRead[a], _],
    TupleWrite[+_[a] <: BoundWrite[a], _],
    Bound[a] <: BoundRead[a] & BoundWrite[a],
    BoundRead[_],
    BoundWrite[_]
] extends TupleableOperation.Read[TupleRead, BoundRead],
      TupleableOperation.Write[TupleWrite, BoundWrite]:
  extension [S[a] <: Bound[a], A](self: S[A])
    def toTuple: Tuple[S, A]

    @targetName("append")
    def :*[T[a] >: S[a] <: Bound[a], B](schema: => T[B])(using append: Append[A, B]): Tuple[T, append.Out]

    @targetName("prepend")
    def *:[T[a] >: S[a] <: Bound[a], B](schema: => T[B])(using prepend: Prepend[A, B]): Tuple[T, prepend.Out]

    @targetName("appendWithRead")
    final def :*[T[a] >: S[a] <: BoundRead[a], B](schema: => T[B])(using
        append: Append[A, B]
    ): TupleRead[T, append.Out] = (self: T[A]) :* schema

    @targetName("appendWithWrite")
    final def :*[T[a] >: S[a] <: BoundWrite[a], B](schema: => T[B])(using
        append: Append[A, B]
    ): TupleWrite[T, append.Out] = (self: T[A]) :* schema

  extension [S[a] >: T[a] <: BoundRead[a], T[a] <: Bound[a], A](self: S[A])
    // @targetName("prependRead")
    def *:[B](schema: => T[B])(using prepend: Prepend[A, B]): TupleRead[S, prepend.Out]

  // extension [S[a] >: SelfWrite[a] <: BoundWrite[a], A](self: S[A])
  //   @targetName("prependWrite")
  //   override def *:[B](schema: => SelfWrite[B])(using prepend: Prepend[A, B]): TupleWrite[S, prepend.Out]

object TupleableOperation:
  trait Read[Tuple[+_[a] <: Bound[a], _], Bound[_]]:
    extension [S[a] <: Bound[a], A](self: S[A])
      @targetName("toTupleRead")
      def toTuple: Tuple[S, A]

      @targetName("appendRead")
      def :*[T[a] >: S[a] <: Bound[a], B](schema: => T[B])(using append: Append[A, B]): Tuple[T, append.Out]

      @targetName("prependRead")
      def *:[T[a] >: S[a] <: Bound[a], B](schema: => T[B])(using prepend: Prepend[A, B]): Tuple[T, prepend.Out]

  trait Write[Tuple[+_[a] <: Bound[a], _], Bound[_]]:
    extension [S[a] <: Bound[a], A](self: S[A])
      @targetName("toTupleWrite")
      def toTuple: Tuple[S, A]

      @targetName("appendWrite")
      def :*[T[a] >: S[a] <: Bound[a], B](schema: => T[B])(using append: Append[A, B]): Tuple[T, append.Out]

      @targetName("prependWrite")
      def *:[T[a] >: S[a] <: Bound[a], B](schema: => T[B])(using prepend: Prepend[A, B]): Tuple[T, prepend.Out]

  def derived[
      Tuple[+s[a] <: Bound[a], a] <: TupleRead[s, a] & TupleWrite[s, a],
      TupleRead[+_[a] <: BoundRead[a], _],
      TupleWrite[+_[a] <: BoundWrite[a], _],
      Bound[a] <: BoundRead[a] & BoundWrite[a],
      BoundRead[_],
      BoundWrite[_]
  ](using
      TupleOperation[Tuple, TupleRead, TupleWrite, Bound, BoundRead, BoundWrite]
  ): TupleableOperation[
    Tuple,
    TupleRead,
    TupleWrite,
    Bound,
    BoundRead,
    BoundWrite
  ] = ???
