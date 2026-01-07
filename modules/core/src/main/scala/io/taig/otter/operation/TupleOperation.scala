package io.taig.otter.operation

import scala.annotation.targetName
import io.taig.otter.Append
import cats.Invariant
import cats.syntax.all.*
import io.taig.otter.Prepend
import cats.Functor
import cats.Contravariant
import io.taig.otter.InvariantK6
import io.taig.otter.InvariantK2

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
  self =>

  def apply[S[a] <: Bound[a], A](schema: => S[A]): Self[S, A]

  override def empty: Self[Nothing, Unit]

  final def imapK[
      SelfK[+s[a] <: Bound[a], a] <: SelfReadK[s, a] & SelfWriteK[s, a],
      SelfReadK[+_[a] <: BoundRead[a], _],
      SelfWriteK[+_[a] <: BoundWrite[a], _]
  ](
      fK: [S[a] <: Bound[a], A] => Self[S, A] => SelfK[S, A],
      gK: [S[a] <: Bound[a], A] => SelfK[S, A] => Self[S, A]
  )(
      fKR: [S[a] <: BoundRead[a], A] => SelfRead[S, A] => SelfReadK[S, A],
      gKR: [S[a] <: BoundRead[a], A] => SelfReadK[S, A] => SelfRead[S, A]
  )(
      fKW: [S[a] <: BoundWrite[a], A] => SelfWrite[S, A] => SelfWriteK[S, A],
      gKW: [S[a] <: BoundWrite[a], A] => SelfWriteK[S, A] => SelfWrite[S, A]
  ): TupleOperation[SelfK, SelfReadK, SelfWriteK, Bound, BoundRead, BoundWrite] =
    new TupleOperation[SelfK, SelfReadK, SelfWriteK, Bound, BoundRead, BoundWrite]:
      override def apply[S[a] <: Bound[a], A](schema: => S[A]): SelfK[S, A] = fK(self.apply(schema))

      override def empty: SelfK[Nothing, Unit] = fK(self.empty)

      extension [S[a] <: Bound[a], T[a] >: S[a] <: Bound[a], A](selfK: SelfK[S, A])
        override def zip[B](schema: SelfK[T, B]): SelfK[T, (A, B)] = fK(self.zip(gK(selfK))(gK(schema)))

      extension [S[a] <: BoundRead[a], T[a] >: S[a] <: BoundRead[a], A](selfK: SelfReadK[S, A])
        @targetName("zipRead")
        override def zip[B](schema: SelfReadK[T, B]): SelfReadK[T, (A, B)] = fKR(self.zip(gKR(selfK))(gKR(schema)))

      extension [S[a] <: BoundWrite[a], T[a] >: S[a] <: BoundWrite[a], A](selfK: SelfWriteK[S, A])
        @targetName("zipWrite")
        override def zip[B](schema: SelfWriteK[T, B]): SelfWriteK[T, (A, B)] = fKW(self.zip(gKW(selfK))(gKW(schema)))

  extension [S[a] <: Bound[a], T[a] >: S[a] <: Bound[a], A](self: Self[S, A])
    def zip[B](schema: Self[T, B]): Self[T, (A, B)]

    @targetName("append")
    final def :*[B](schema: T[B])(using append: Append[A, B])(using Invariant[Self[T, *]]): Self[T, append.Out] =
      self.zip(apply(schema)).imap(append.apply)(append.unapply)

  extension [S[a] <: Bound[a], T[a] >: S[a] <: BoundRead[a], A](self: Self[S, A])
    @targetName("appendWithRead")
    final def :*[B](schema: T[B])(using append: Append[A, B])(using Functor[SelfRead[T, *]]): SelfRead[T, append.Out] =
      (self: SelfRead[S, A]) :* schema

  extension [S[a] <: Bound[a], T[a] >: S[a] <: BoundWrite[a], A](self: Self[S, A])
    @targetName("appendWithWrite")
    final def :*[B](schema: T[B])(using
        append: Append[A, B]
    )(using Contravariant[SelfWrite[T, *]]): SelfWrite[T, append.Out] = (self: SelfWrite[S, A]) :* schema

  extension [S[a] >: T[a] <: Bound[a], T[a] <: Bound[a], A](self: S[A])
    @targetName("prepend")
    final def *:[B](schema: Self[T, B])(using
        prepend: Prepend[A, B]
    )(using Invariant[Self[S, *]]): Self[S, prepend.Out] = apply(self).zip(schema).imap(prepend.apply)(prepend.unapply)

  extension [S[a] >: T[a] <: BoundRead[a], T[a] <: Bound[a], A](self: S[A])
    @targetName("prependWithRead")
    final def *:[B](schema: Self[T, B])(using
        prepend: Prepend[A, B]
    )(using Functor[SelfRead[S, *]]): SelfRead[S, prepend.Out] =
      self *: (schema: SelfRead[T, B])

  extension [S[a] >: T[a] <: BoundWrite[a], T[a] <: Bound[a], A](self: S[A])
    @targetName("prependWithWrite")
    final def *:[B](schema: Self[T, B])(using
        prepend: Prepend[A, B]
    )(using Contravariant[SelfWrite[S, *]]): SelfWrite[S, prepend.Out] = self *: (schema: SelfWrite[T, B])

object TupleOperation:
  trait Read[Self[+_[a] <: Bound[a], _], Bound[_]] extends Operation.Read[Self, Bound]:
    self =>

    @targetName("applyRead")
    def apply[S[a] <: Bound[a], A](schema: => S[A]): Self[S, A]

    def empty: Self[Nothing, Unit]

    extension [S[a] <: Bound[a], T[a] >: S[a] <: Bound[a], A](self: Self[S, A])
      @targetName("zipRead")
      def zip[B](schema: Self[T, B]): Self[T, (A, B)]

      @targetName("appendRead")
      final def :*[B](schema: T[B])(using append: Append[A, B])(using Functor[Self[T, *]]): Self[T, append.Out] =
        self.zip(apply(schema)).map(append.apply)

    extension [S[a] >: T[a] <: Bound[a], T[a] <: Bound[a], A](self: S[A])
      @targetName("prependRead")
      final def *:[B](schema: Self[T, B])(using
          prepend: Prepend[A, B]
      )(using Functor[Self[S, *]]): Self[S, prepend.Out] = apply(self).zip(schema).map(prepend.apply)

    final def imapK[H[+_[a] <: Bound[a], _]](fK: [S[a] <: Bound[a], A] => Self[S, A] => H[S, A])(
        gK: [S[a] <: Bound[a], A] => H[S, A] => Self[S, A]
    ): TupleOperation.Read[H, Bound] = new TupleOperation.Read[H, Bound]:
      @targetName("applyRead")
      def apply[S[a] <: Bound[a], A](schema: => S[A]): H[S, A] = fK(self.apply(schema))

      def empty: H[Nothing, Unit] = fK(self.empty)

      extension [S[a] <: Bound[a], T[a] >: S[a] <: Bound[a], A](selfH: H[S, A])
        @targetName("zipRead")
        def zip[B](schema: H[T, B]): H[T, (A, B)] = fK(self.zip(gK(selfH))(gK(schema)))

  object Read:
    inline def apply[Self[+_[a] <: Bound[a], _], Bound[_]](using
        self: TupleOperation.Read[Self, Bound]
    ): TupleOperation.Read[Self, Bound] = self

    given InvariantK2[TupleOperation.Read]:
      extension [H[+_[a] <: G[a], _], G[_]](fa: TupleOperation.Read[H, G])
        def imapK[I[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => H[S, A] => I[S, A])(
            gK: [S[a] <: G[a], A] => I[S, A] => H[S, A]
        ): TupleOperation.Read[I, G] = fa.imapK(fK)(gK)

  trait Write[Self[+_[a] <: Bound[a], _], Bound[_]] extends Operation.Write[Self, Bound]:
    self =>

    @targetName("applyWrite")
    def apply[S[a] <: Bound[a], A](schema: => S[A]): Self[S, A]

    def empty: Self[Nothing, Unit]

    extension [S[a] <: Bound[a], T[a] >: S[a] <: Bound[a], A](self: Self[S, A])
      @targetName("zipWrite")
      def zip[B](schema: Self[T, B]): Self[T, (A, B)]

      @targetName("appendWrite")
      final def :*[B](schema: T[B])(using append: Append[A, B])(using Contravariant[Self[T, *]]): Self[T, append.Out] =
        self.zip(apply(schema)).contramap(append.unapply)

    extension [S[a] >: T[a] <: Bound[a], T[a] <: Bound[a], A](self: S[A])
      @targetName("prependWrite")
      final def *:[B](schema: Self[T, B])(using
          prepend: Prepend[A, B]
      )(using Contravariant[Self[S, *]]): Self[S, prepend.Out] = apply(self).zip(schema).contramap(prepend.unapply)

    final def imapK[H[+_[a] <: Bound[a], _]](fK: [S[a] <: Bound[a], A] => Self[S, A] => H[S, A])(
        gK: [S[a] <: Bound[a], A] => H[S, A] => Self[S, A]
    ): TupleOperation.Write[H, Bound] = new TupleOperation.Write[H, Bound]:
      @targetName("applyWrite")
      def apply[S[a] <: Bound[a], A](schema: => S[A]): H[S, A] = fK(self.apply(schema))

      def empty: H[Nothing, Unit] = fK(self.empty)

      extension [S[a] <: Bound[a], T[a] >: S[a] <: Bound[a], A](selfH: H[S, A])
        @targetName("zipWrite")
        def zip[B](schema: H[T, B]): H[T, (A, B)] = fK(self.zip(gK(selfH))(gK(schema)))

  object Write:
    inline def apply[Self[+_[a] <: Bound[a], _], Bound[_]](using
        self: TupleOperation.Write[Self, Bound]
    ): TupleOperation.Write[Self, Bound] = self

    given InvariantK2[TupleOperation.Write]:
      extension [H[+_[a] <: G[a], _], G[_]](fa: TupleOperation.Write[H, G])
        def imapK[I[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => H[S, A] => I[S, A])(
            gK: [S[a] <: G[a], A] => I[S, A] => H[S, A]
        ): TupleOperation.Write[I, G] = fa.imapK(fK)(gK)

  inline def apply[
      Self[+s[a] <: Bound[a], a] <: SelfRead[s, a] & SelfWrite[s, a],
      SelfRead[+_[a] <: BoundRead[a], _],
      SelfWrite[+_[a] <: BoundWrite[a], _],
      Bound[a] <: BoundRead[a] & BoundWrite[a],
      BoundRead[_],
      BoundWrite[_]
  ](using
      self: TupleOperation[Self, SelfRead, SelfWrite, Bound, BoundRead, BoundWrite]
  ): TupleOperation[Self, SelfRead, SelfWrite, Bound, BoundRead, BoundWrite] = self

  given InvariantK6[TupleOperation]:
    extension [
        Self[+s[a] <: Bound[a], a] <: SelfRead[s, a] & SelfWrite[s, a],
        SelfRead[+_[a] <: BoundRead[a], _],
        SelfWrite[+_[a] <: BoundWrite[a], _],
        Bound[a] <: BoundRead[a] & BoundWrite[a],
        BoundRead[_],
        BoundWrite[_]
    ](fa: TupleOperation[Self, SelfRead, SelfWrite, Bound, BoundRead, BoundWrite])
      def imapK[
          SelfK[+s[a] <: Bound[a], a] <: SelfReadK[s, a] & SelfWriteK[s, a],
          SelfReadK[+_[a] <: BoundRead[a], _],
          SelfWriteK[+_[a] <: BoundWrite[a], _]
      ](
          fK: [S[a] <: Bound[a], A] => Self[S, A] => SelfK[S, A],
          gK: [S[a] <: Bound[a], A] => SelfK[S, A] => Self[S, A]
      )(
          fKR: [S[a] <: BoundRead[a], A] => SelfRead[S, A] => SelfReadK[S, A],
          gKR: [S[a] <: BoundRead[a], A] => SelfReadK[S, A] => SelfRead[S, A]
      )(
          fKW: [S[a] <: BoundWrite[a], A] => SelfWrite[S, A] => SelfWriteK[S, A],
          gKW: [S[a] <: BoundWrite[a], A] => SelfWriteK[S, A] => SelfWrite[S, A]
      ): TupleOperation[SelfK, SelfReadK, SelfWriteK, Bound, BoundRead, BoundWrite] =
        fa.imapK[SelfK, SelfReadK, SelfWriteK](fK, gK)(fKR, gKR)(fKW, gKW)
