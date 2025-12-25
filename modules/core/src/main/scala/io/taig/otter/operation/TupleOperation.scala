package io.taig.otter.operation

import scala.annotation.targetName
import io.taig.otter.InvariantK3
import io.taig.otter.Reference
import io.taig.otter.InvariantK9

abstract class TupleOperation[
    Self[+s[a] <: Bound[a], a] <: SelfRead[s, a] & SelfWrite[s, a],
    SelfRead[+_[a] <: BoundRead[a], _],
    SelfWrite[+_[a] <: BoundWrite[a], _],
    Schema[+_[a] <: Bound[a], a] <: Bound[a],
    SchemaRead[+_[a] <: BoundRead[a], a] <: BoundRead[a],
    SchemaWrite[+_[a] <: BoundWrite[a], a] <: BoundWrite[a],
    Bound[a] <: BoundRead[a] & BoundWrite[a],
    BoundRead[_],
    BoundWrite[_]
] extends TupleOperation.Read[SelfRead, SchemaRead, BoundRead],
      TupleOperation.Write[SelfWrite, SchemaWrite, BoundWrite]:
  self =>

  def apply[S[+_[a] <: Bound[a], a] <: Bound[a], T[a] <: Bound[a], A](
      schema: Reference[S[T, *], A]
  ): Self[S[T, *], A]

  override def empty: Self[Nothing, Unit]

  extension [F[a] <: Bound[a], A](self: Self[F, A])
    def zip[G[a] >: F[a] <: Bound[a], B](schema: Self[G, B]): Self[G, (A, B)]

    // final def :*[S[+_[a] <: Bound[a], a] >: F[a] <: Bound[a], T[a] >: F[a] <: Bound[a], B](schema: => S[T, B])(using
    //     append: Append[A, B]
    // )(using Invariant[Self[S[T, *], *]]): Self[S[T, *], append.Out] =
    //   self.zip(apply(schema = Reference.later(schema))).imap(append.apply)(append.unapply)

    // final def *:[S[+_[a] <: Bound[a], a] >: F[a] <: Bound[a], T[a] >: F[a] <: Bound[a], B](schema: => S[T, B])(using
    //     prepend: Prepend[A, B]
    // )(using Invariant[Self[S[T, *], *]]): Self[S[T, *], prepend.Out] =
    //   self.zip(apply(schema = Reference.later(schema))).imap(prepend.apply)(prepend.unapply)

  extension [F[a] <: Bound[a], A](self: Self[F, A])
    @targetName("zipWithRead")
    final def zip[G[a] >: F[a] <: BoundRead[a], B](schema: SelfRead[G, B]): SelfRead[G, (A, B)] =
      (self: SelfRead[F, A]).zip(schema)

    @targetName("zipWithWrite")
    final def zip[G[a] >: F[a] <: BoundWrite[a], B](schema: SelfWrite[G, B]): SelfWrite[G, (A, B)] =
      (self: SelfWrite[F, A]).zip(schema)

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
  ): TupleOperation[SelfK, SelfReadK, SelfWriteK, Schema, SchemaRead, SchemaWrite, Bound, BoundRead, BoundWrite] =
    new TupleOperation[SelfK, SelfReadK, SelfWriteK, Schema, SchemaRead, SchemaWrite, Bound, BoundRead, BoundWrite]:
      override def apply[S[+_[a] <: Bound[a], a] <: Bound[a], T[a] <: Bound[a], A](
          schema: Reference[S[T, *], A]
      ): SelfK[S[T, *], A] = fK(self.apply(schema))

      override def empty: SelfK[Nothing, Unit] = fK(self.empty)

      extension [F[a] <: Bound[a], A](sfa: SelfK[F, A])
        def zip[G[a] >: F[a] <: Bound[a], B](schema: SelfK[G, B]): SelfK[G, (A, B)] =
          fK(self.zip(gK(sfa))(gK(schema)))

      extension [F[a] <: BoundRead[a], A](sfa: SelfReadK[F, A])
        @targetName("zipRead")
        def zip[G[a] >: F[a] <: BoundRead[a], B](schema: SelfReadK[G, B]): SelfReadK[G, (A, B)] =
          fKR(self.zip(gKR(sfa))(gKR(schema)))

      extension [F[a] <: BoundWrite[a], A](sfa: SelfWriteK[F, A])
        @targetName("zipWrite")
        def zip[G[a] >: F[a] <: BoundWrite[a], B](schema: SelfWriteK[G, B]): SelfWriteK[G, (A, B)] =
          fKW(self.zip(gKW(sfa))(gKW(schema)))

object TupleOperation:
  trait Read[Self[+_[a] <: Bound[a], a], Schema[+_[a] <: Bound[a], a] <: Bound[a], Bound[_]]
      extends Operation.Read[Self, Bound]:
    self =>

    @targetName("applyRead")
    def apply[S[+s[a] <: Bound[a], a] <: Schema[s, a], T[a] <: Bound[a], A](
        schema: Reference[S[T, *], A]
    ): Self[S[T, *], A]

    def empty: Self[Nothing, Unit]

    extension [F[a] <: Bound[a], A](self: Self[F, A])
      @targetName("zipRead")
      def zip[G[a] >: F[a] <: Bound[a], B](schema: Self[G, B]): Self[G, (A, B)]

    final def imapK[SelfK[+_[a] <: Bound[a], _]](
        fK: [S[a] <: Bound[a], A] => Self[S, A] => SelfK[S, A],
        gK: [S[a] <: Bound[a], A] => SelfK[S, A] => Self[S, A]
    ): TupleOperation.Read[SelfK, Schema, Bound] = new Read[SelfK, Schema, Bound]:
      @targetName("applyRead")
      override def apply[S[+s[a] <: Bound[a], a] <: Schema[s, a], T[a] <: Bound[a], A](
          schema: Reference[S[T, *], A]
      ): SelfK[S[T, *], A] = fK(self.apply(schema))

      override def empty: SelfK[Nothing, Unit] = fK(self.empty)

      extension [F[a] <: Bound[a], A](sfa: SelfK[F, A])
        @targetName("zipRead")
        override def zip[G[a] >: F[a] <: Bound[a], B](schema: SelfK[G, B]): SelfK[G, (A, B)] =
          fK(self.zip(gK(sfa))(gK(schema)))

  object Read:
    inline def apply[Self[+_[a] <: Bound[a], a] <: Bound[a], Schema[+_[a] <: Bound[a], a] <: Bound[a], Bound[_]](using
        self: TupleOperation.Read[Self, Schema, Bound]
    ): TupleOperation.Read[Self, Schema, Bound] = self

    given InvariantK3[TupleOperation.Read]:
      extension [H[+_[a] <: K[a], _], G[+_[a] <: K[a], a] <: K[a], K[_]](fa: TupleOperation.Read[H, G, K])
        override def imapK[I[+_[a] <: K[a], _]](fK: [S[a] <: K[a], A] => H[S, A] => I[S, A])(
            gK: [S[a] <: K[a], A] => I[S, A] => H[S, A]
        ): TupleOperation.Read[I, G, K] = fa.imapK(fK, gK)

  trait Write[Self[+_[a] <: Bound[a], _], Schema[+_[a] <: Bound[a], a] <: Bound[a], Bound[_]]
      extends Operation.Write[Self, Bound]:
    self =>

    @targetName("applyWrite")
    def apply[S[+s[a] <: Bound[a], a] <: Schema[s, a], T[a] <: Bound[a], A](
        schema: Reference[S[T, *], A]
    ): Self[S[T, *], A]

    def empty: Self[Nothing, Unit]

    extension [F[a] <: Bound[a], A](self: Self[F, A])
      @targetName("zipWrite")
      def zip[G[a] >: F[a] <: Bound[a], B](schema: Self[G, B]): Self[G, (A, B)]

    final def imapK[SelfK[+_[a] <: Bound[a], _]](
        fK: [S[a] <: Bound[a], A] => Self[S, A] => SelfK[S, A],
        gK: [S[a] <: Bound[a], A] => SelfK[S, A] => Self[S, A]
    ): TupleOperation.Write[SelfK, Schema, Bound] = new Write[SelfK, Schema, Bound]:
      @targetName("applyWrite")
      override def apply[S[+s[a] <: Bound[a], a] <: Schema[s, a], T[a] <: Bound[a], A](
          schema: Reference[S[T, *], A]
      ): SelfK[S[T, *], A] = fK(self.apply(schema))

      override def empty: SelfK[Nothing, Unit] = fK(self.empty)

      extension [F[a] <: Bound[a], A](sfa: SelfK[F, A])
        @targetName("zipWrite")
        override def zip[G[a] >: F[a] <: Bound[a], B](schema: SelfK[G, B]): SelfK[G, (A, B)] =
          fK(self.zip(gK(sfa))(gK(schema)))

  object Write:
    inline def apply[
        Self[+_[a] <: Bound[a], _],
        Schema[+_[a] <: Bound[a], a] <: Bound[a],
        Bound[_]
    ](using self: TupleOperation.Write[Self, Schema, Bound]): TupleOperation.Write[Self, Schema, Bound] = self

    given InvariantK3[TupleOperation.Write]:
      extension [H[+_[a] <: K[a], _], G[+_[a] <: K[a], a] <: K[a], K[_]](fa: TupleOperation.Write[H, G, K])
        override def imapK[I[+_[a] <: K[a], _]](fK: [S[a] <: K[a], A] => H[S, A] => I[S, A])(
            gK: [S[a] <: K[a], A] => I[S, A] => H[S, A]
        ): TupleOperation.Write[I, G, K] = fa.imapK(fK, gK)

  inline def apply[
      Self[+s[a] <: Bound[a], a] <: SelfRead[s, a] & SelfWrite[s, a],
      SelfRead[+_[a] <: BoundRead[a], _],
      SelfWrite[+_[a] <: BoundWrite[a], _],
      Schema[+_[a] <: Bound[a], a] <: Bound[a],
      SchemaRead[+_[a] <: BoundRead[a], a] <: BoundRead[a],
      SchemaWrite[+_[a] <: BoundWrite[a], a] <: BoundWrite[a],
      Bound[a] <: BoundRead[a] & BoundWrite[a],
      BoundRead[_],
      BoundWrite[_]
  ](using
      self: TupleOperation[Self, SelfRead, SelfWrite, Schema, SchemaRead, SchemaWrite, Bound, BoundRead, BoundWrite]
  ): TupleOperation[Self, SelfRead, SelfWrite, Schema, SchemaRead, SchemaWrite, Bound, BoundRead, BoundWrite] = self

  given InvariantK9[TupleOperation]:
    extension [
        Self[+s[a] <: Bound[a], a] <: SelfRead[s, a] & SelfWrite[s, a],
        SelfRead[+_[a] <: BoundRead[a], _],
        SelfWrite[+_[a] <: BoundWrite[a], _],
        Schema[+_[a] <: Bound[a], a] <: Bound[a],
        SchemaRead[+_[a] <: BoundRead[a], a] <: BoundRead[a],
        SchemaWrite[+_[a] <: BoundWrite[a], a] <: BoundWrite[a],
        Bound[a] <: BoundRead[a] & BoundWrite[a],
        BoundRead[_],
        BoundWrite[_]
    ](fa: TupleOperation[Self, SelfRead, SelfWrite, Schema, SchemaRead, SchemaWrite, Bound, BoundRead, BoundWrite])
      override def imapK[
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
      ): TupleOperation[SelfK, SelfReadK, SelfWriteK, Schema, SchemaRead, SchemaWrite, Bound, BoundRead, BoundWrite] =
        fa.imapK[SelfK, SelfReadK, SelfWriteK](fK, gK)(fKR, gKR)(fKW, gKW)
