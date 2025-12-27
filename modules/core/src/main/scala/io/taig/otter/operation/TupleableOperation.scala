package io.taig.otter.operation

import scala.annotation.targetName

abstract class TupleableOperation[
    Tuple[+_[a] <: Bound[a], _],
    TupleRead[+_[a] <: BoundRead[a], _],
    TupleWrite[+_[a] <: BoundWrite[a], _],
    Bound[_],
    BoundRead[_],
    BoundWrite[_]
] extends TupleableOperation.Read[TupleRead, BoundRead],
      TupleableOperation.Write[TupleWrite, BoundWrite]:
  extension [S[a] <: Bound[a], A](self: S[A]) def toTuple: Tuple[S, A]

  //   @targetName("append")
  //   def :*[S[a] >: Self[a] <: Bound[a], B](schema: => S[B])(using append: Append[A, B]): Tuple[S, append.Out]

  //   @targetName("prepend")
  //   def *:[S[a] >: Self[a] <: Bound[a], B](schema: => S[B])(using prepend: Prepend[A, B]): Tuple[S, prepend.Out]

  //   @targetName("appendWithRead")
  //   final def :*[S[a] >: SelfRead[a] <: BoundRead[a], B](schema: => S[B])(using
  //       append: Append[A, B]
  //   ): TupleRead[S, append.Out] = (self: SelfRead[A]) :* schema

  //   @targetName("appendWithWrite")
  //   final def :*[S[a] >: SelfWrite[a] <: BoundWrite[a], B](schema: => S[B])(using
  //       append: Append[A, B]
  //   ): TupleWrite[S, append.Out] = (self: SelfWrite[A]) :* schema

  // extension [S[a] >: SelfRead[a] <: BoundRead[a], A](self: S[A])
  //   @targetName("prependRead")
  //   override def *:[B](schema: => SelfRead[B])(using prepend: Prepend[A, B]): TupleRead[S, prepend.Out]

  // extension [S[a] >: SelfWrite[a] <: BoundWrite[a], A](self: S[A])
  //   @targetName("prependWrite")
  //   override def *:[B](schema: => SelfWrite[B])(using prepend: Prepend[A, B]): TupleWrite[S, prepend.Out]

object TupleableOperation:
  trait Read[Tuple[+_[a] <: Bound[a], _], Bound[_]]:
    extension [S[a] <: Bound[a], A](self: S[A])
      @targetName("toTupleRead")
      def toTuple: Tuple[S, A]

  //   @targetName("appendRead")
  //   def :*[S[a] >: Self[a] <: Bound[a], B](schema: => S[B])(using append: Append[A, B]): Tuple[S, append.Out]

  // extension [S[a] >: Self[a] <: Bound[a], A](self: S[A])
  //   @targetName("prependRead")
  //   def *:[B](schema: => Self[B])(using prepend: Prepend[A, B]): Tuple[S, prepend.Out]

  trait Write[Tuple[+_[a] <: Bound[a], _], Bound[_]]:
    extension [S[a] <: Bound[a], A](self: S[A])
      @targetName("toTupleWrite")
      def toTuple: Tuple[S, A]

  //   @targetName("appendWrite")
  //   def :*[S[a] >: Self[a] <: Bound[a], B](schema: => S[B])(using append: Append[A, B]): Tuple[S, append.Out]

  // extension [S[a] >: Self[a] <: Bound[a], A](self: S[A])
  //   @targetName("prependWrite")
  //   def *:[B](schema: => Self[B])(using prepend: Prepend[A, B]): Tuple[S, prepend.Out]

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
