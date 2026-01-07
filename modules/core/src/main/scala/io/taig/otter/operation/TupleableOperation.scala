package io.taig.otter.operation

import scala.annotation.targetName
import io.taig.otter.Append
import cats.Functor
import cats.syntax.all.*
import io.taig.otter.Prepend
import cats.Contravariant
import cats.InvariantSemigroupal

abstract class TupleableOperation[
    Tuple[+_[a] <: Bound[a], _],
    TupleRead[+_[a] <: BoundRead[a], _],
    TupleWrite[+_[a] <: BoundWrite[a], _],
    Bound[a] <: BoundRead[a] & BoundWrite[a],
    BoundRead[_],
    BoundWrite[_]
] extends TupleableOperation.Read[TupleRead, BoundRead],
      TupleableOperation.Write[TupleWrite, BoundWrite]:
  extension [S[a] <: Bound[a], A](self: S[A]) def toTuple: Tuple[S, A]

  extension [S[a] <: Bound[a], T[a] >: S[a] <: Bound[a], A](self: S[A])
    @targetName("append")
    def :*[B](schema: => T[B])(using InvariantSemigroupal[Tuple[T, *]]): Tuple[T, Append[A, B]]

  extension [S[a] <: Bound[a], T[a] >: S[a] <: BoundRead[a], A](self: S[A])
    @targetName("appendWithRead")
    final def :*[B](schema: => T[B])(using Functor[TupleRead[T, *]]): TupleRead[T, Append[A, B]] =
      (self: T[A]) :* schema

  extension [S[a] <: Bound[a], T[a] >: S[a] <: BoundWrite[a], A](self: S[A])
    @targetName("appendWithWrite")
    final def :*[B](schema: => T[B])(using Contravariant[TupleWrite[T, *]]): TupleWrite[T, Append[A, B]] =
      (self: T[A]) :* schema

  extension [S[a] >: T[a] <: Bound[a], T[a] <: Bound[a], A](self: S[A])
    @targetName("prepend")
    def *:[B](schema: => T[B])(using InvariantSemigroupal[Tuple[S, *]]): Tuple[S, Prepend[A, B]]

  extension [S[a] >: T[a] <: BoundRead[a], T[a] <: Bound[a], A](self: S[A])
    @targetName("prependWithRead")
    final def *:[B](schema: => T[B])(using Functor[TupleRead[S, *]]): TupleRead[S, Prepend[A, B]] =
      self *: (schema: S[B])

  extension [S[a] >: T[a] <: BoundWrite[a], T[a] <: Bound[a], A](self: S[A])
    @targetName("prependWithWrite")
    final def *:[B](schema: => T[B])(using Contravariant[TupleWrite[S, *]]): TupleWrite[S, Prepend[A, B]] =
      self *: (schema: S[B])

object TupleableOperation:
  trait Read[Tuple[+_[a] <: Bound[a], _], Bound[_]]:
    extension [S[a] <: Bound[a], A](self: S[A])
      @targetName("toTupleRead")
      def toTuple: Tuple[S, A]

    extension [S[a] <: Bound[a], T[a] >: S[a] <: Bound[a], A](self: S[A])
      @targetName("appendRead")
      def :*[B](schema: => T[B])(using Functor[Tuple[T, *]]): Tuple[T, Append[A, B]]

    extension [S[a] >: T[a] <: Bound[a], T[a] <: Bound[a], A](self: S[A])
      @targetName("prependRead")
      def *:[B](schema: => T[B])(using Functor[Tuple[S, *]]): Tuple[S, Prepend[A, B]]

  trait Write[Tuple[+_[a] <: Bound[a], _], Bound[_]]:
    extension [S[a] <: Bound[a], A](self: S[A])
      @targetName("toTupleWrite")
      def toTuple: Tuple[S, A]

    extension [S[a] <: Bound[a], T[a] >: S[a] <: Bound[a], A](self: S[A])
      @targetName("appendWrite")
      def :*[B](schema: => T[B])(using Contravariant[Tuple[T, *]]): Tuple[T, Append[A, B]]

    extension [S[a] >: T[a] <: Bound[a], T[a] <: Bound[a], A](self: S[A])
      @targetName("prependWrite")
      def *:[B](schema: => T[B])(using Contravariant[Tuple[S, *]]): Tuple[S, Prepend[A, B]]

  def derived[
      Tuple[+s[a] <: Bound[a], a] <: TupleRead[s, a] & TupleWrite[s, a] & Matchable,
      TupleRead[+_[a] <: BoundRead[a], _] <: Matchable,
      TupleWrite[+_[a] <: BoundWrite[a], _] <: Matchable,
      Bound[a] <: BoundRead[a] & BoundWrite[a],
      BoundRead[_],
      BoundWrite[_]
  ](using
      F: TupleOperation[Tuple, TupleRead, TupleWrite, Bound, BoundRead, BoundWrite]
  ): TupleableOperation[
    Tuple,
    TupleRead,
    TupleWrite,
    Bound,
    BoundRead,
    BoundWrite
  ] = new TupleableOperation[
    Tuple,
    TupleRead,
    TupleWrite,
    Bound,
    BoundRead,
    BoundWrite
  ]:
    extension [S[a] <: Bound[a], A](self: S[A]) override def toTuple: Tuple[S, A] = F.apply(self)

    extension [S[a] <: Bound[a], T[a] >: S[a] <: Bound[a], A](self: S[A])
      @targetName("append")
      override def :*[B](schema: => T[B])(using InvariantSemigroupal[Tuple[T, *]]): Tuple[T, Append[A, B]] =
        Append(self.toTuple, schema.toTuple)

    extension [S[a] >: T[a] <: Bound[a], T[a] <: Bound[a], A](self: S[A])
      @targetName("prepend")
      override def *:[B](schema: => T[B])(using InvariantSemigroupal[Tuple[S, *]]): Tuple[S, Prepend[A, B]] =
        Prepend(self.toTuple, schema.toTuple)
