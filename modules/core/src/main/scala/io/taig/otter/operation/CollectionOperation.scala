package io.taig.otter.operation

import scala.annotation.targetName
import cats.data.Chain
import io.taig.validation.Validation
import io.taig.otter.Constraint
import io.taig.otter.InvariantK2
import io.taig.otter.InvariantK6

abstract class CollectionOperation[
    Self[+s[a] <: Bound[a], a] <: SelfRead[s, a] & SelfWrite[s, a],
    SelfRead[+_[a] <: BoundRead[a], _],
    SelfWrite[+_[a] <: BoundWrite[a], _],
    Bound[a] <: BoundRead[a] & BoundWrite[a],
    BoundRead[_],
    BoundWrite[_]
] extends Operation[Self, SelfRead, SelfWrite, Bound, BoundRead, BoundWrite],
      CollectionOperation.Read[SelfRead, BoundRead],
      CollectionOperation.Write[SelfWrite, BoundWrite]:
  self =>

  def chained[S[a] <: Bound[a], A](
      schema: => S[A],
      validation: Validation[Constraint.Collection, Chain[A]]
  ): Self[S, Chain[A]]

  def indexed[S[a] <: Bound[a], A](
      schema: => S[A],
      validation: Validation[Constraint.Collection, Vector[A]]
  ): Self[S, Vector[A]]

  def linked[S[a] <: Bound[a], A](
      schema: => S[A],
      validation: Validation[Constraint.Collection, List[A]]
  ): Self[S, List[A]]

  final def mapK[
      SelfK[+s[a] <: Bound[a], a] <: SelfReadK[s, a] & SelfWriteK[s, a],
      SelfReadK[+_[a] <: BoundRead[a], _],
      SelfWriteK[+_[a] <: BoundWrite[a], _]
  ](
      fK: [S[a] <: Bound[a], A] => Self[S, A] => SelfK[S, A]
  )(
      fKR: [S[a] <: BoundRead[a], A] => SelfRead[S, A] => SelfReadK[S, A]
  )(
      fKW: [S[a] <: BoundWrite[a], A] => SelfWrite[S, A] => SelfWriteK[S, A]
  ): CollectionOperation[SelfK, SelfReadK, SelfWriteK, Bound, BoundRead, BoundWrite] =
    new CollectionOperation[SelfK, SelfReadK, SelfWriteK, Bound, BoundRead, BoundWrite]:
      override def chained[S[a] <: Bound[a], A](
          schema: => S[A],
          validation: Validation[Constraint.Collection, Chain[A]]
      ): SelfK[S, Chain[A]] = fK(self.chained(schema, validation))

      override def indexed[S[a] <: Bound[a], A](
          schema: => S[A],
          validation: Validation[Constraint.Collection, Vector[A]]
      ): SelfK[S, Vector[A]] = fK(self.indexed(schema, validation))

      override def linked[S[a] <: Bound[a], A](
          schema: => S[A],
          validation: Validation[Constraint.Collection, List[A]]
      ): SelfK[S, List[A]] = fK(self.linked(schema, validation))

      @targetName("chainedRead")
      override def chained[S[a] <: BoundRead[a], A](
          schema: => S[A],
          validation: Validation[Constraint.Collection, Chain[A]]
      ): SelfReadK[S, Chain[A]] = fKR(self.chained(schema, validation))

      @targetName("indexedRead")
      override def indexed[S[a] <: BoundRead[a], A](
          schema: => S[A],
          validation: Validation[Constraint.Collection, Vector[A]]
      ): SelfReadK[S, Vector[A]] = fKR(self.indexed(schema, validation))

      @targetName("linkedRead")
      override def linked[S[a] <: BoundRead[a], A](
          schema: => S[A],
          validation: Validation[Constraint.Collection, List[A]]
      ): SelfReadK[S, List[A]] = fKR(self.linked(schema, validation))

      @targetName("chainedWrite")
      override def chained[S[a] <: BoundWrite[a], A](schema: => S[A]): SelfWriteK[S, Chain[A]] =
        fKW((self: CollectionOperation.Write[SelfWrite, BoundWrite]).chained(schema))

      @targetName("indexedWrite")
      override def indexed[S[a] <: BoundWrite[a], A](schema: => S[A]): SelfWriteK[S, Vector[A]] =
        fKW((self: CollectionOperation.Write[SelfWrite, BoundWrite]).indexed(schema))

      @targetName("linkedWrite")
      override def linked[S[a] <: BoundWrite[a], A](schema: => S[A]): SelfWriteK[S, List[A]] =
        fKW((self: CollectionOperation.Write[SelfWrite, BoundWrite]).linked(schema))

object CollectionOperation:
  trait Read[Self[+_[a] <: Bound[a], _], Bound[_]] extends Operation.Read[Self, Bound]:
    self =>

    @targetName("chainedRead")
    def chained[S[a] <: Bound[a], A](
        schema: => S[A],
        validation: Validation[Constraint.Collection, Chain[A]]
    ): Self[S, Chain[A]]

    @targetName("indexedRead")
    def indexed[S[a] <: Bound[a], A](
        schema: => S[A],
        validation: Validation[Constraint.Collection, Vector[A]]
    ): Self[S, Vector[A]]

    @targetName("linkedRead")
    def linked[S[a] <: Bound[a], A](
        schema: => S[A],
        validation: Validation[Constraint.Collection, List[A]]
    ): Self[S, List[A]]

    final def mapK[H[+_[a] <: Bound[a], _]](
        fK: [S[a] <: Bound[a], A] => Self[S, A] => H[S, A]
    ): CollectionOperation.Read[H, Bound] =
      new CollectionOperation.Read[H, Bound]:
        @targetName("chainedRead")
        def chained[S[a] <: Bound[a], A](
            schema: => S[A],
            validation: Validation[Constraint.Collection, Chain[A]]
        ): H[S, Chain[A]] = fK(self.chained(schema, validation))

        @targetName("indexedRead")
        def indexed[S[a] <: Bound[a], A](
            schema: => S[A],
            validation: Validation[Constraint.Collection, Vector[A]]
        ): H[S, Vector[A]] = fK(self.indexed(schema, validation))

        @targetName("linkedRead")
        def linked[S[a] <: Bound[a], A](
            schema: => S[A],
            validation: Validation[Constraint.Collection, List[A]]
        ): H[S, List[A]] = fK(self.linked(schema, validation))

  object Read:
    inline def apply[Self[+_[a] <: Bound[a], _], Bound[_]](using
        self: CollectionOperation.Read[Self, Bound]
    ): CollectionOperation.Read[Self, Bound] = self

    given InvariantK2[CollectionOperation.Read]:
      extension [H[+_[a] <: G[a], _], G[_]](fa: CollectionOperation.Read[H, G])
        def imapK[I[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => H[S, A] => I[S, A])(
            gK: [S[a] <: G[a], A] => I[S, A] => H[S, A]
        ): CollectionOperation.Read[I, G] = fa.mapK(fK)

  trait Write[Self[+_[a] <: Bound[a], _], Bound[_]] extends Operation.Write[Self, Bound]:
    self =>

    @targetName("chainedWrite")
    def chained[S[a] <: Bound[a], A](schema: => S[A]): Self[S, Chain[A]]

    @targetName("indexedWrite")
    def indexed[S[a] <: Bound[a], A](schema: => S[A]): Self[S, Vector[A]]

    @targetName("linkedWrite")
    def linked[S[a] <: Bound[a], A](schema: => S[A]): Self[S, List[A]]

    final def mapK[H[+_[a] <: Bound[a], _]](
        fK: [S[a] <: Bound[a], A] => Self[S, A] => H[S, A]
    ): CollectionOperation.Write[H, Bound] = new CollectionOperation.Write[H, Bound]:
      @targetName("chainedWrite")
      def chained[S[a] <: Bound[a], A](schema: => S[A]): H[S, Chain[A]] =
        fK(self.chained(schema))

      @targetName("indexedWrite")
      def indexed[S[a] <: Bound[a], A](schema: => S[A]): H[S, Vector[A]] =
        fK(self.indexed(schema))

      @targetName("linkedWrite")
      def linked[S[a] <: Bound[a], A](schema: => S[A]): H[S, List[A]] = fK(self.linked(schema))

  object Write:
    inline def apply[Self[+_[a] <: Bound[a], _], Bound[_]](using
        self: CollectionOperation.Write[Self, Bound]
    ): CollectionOperation.Write[Self, Bound] = self

    given InvariantK2[CollectionOperation.Write]:
      extension [H[+_[a] <: G[a], _], G[_]](fa: CollectionOperation.Write[H, G])
        def imapK[I[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => H[S, A] => I[S, A])(
            gK: [S[a] <: G[a], A] => I[S, A] => H[S, A]
        ): CollectionOperation.Write[I, G] = fa.mapK(fK)

  inline def apply[
      Self[+s[a] <: Bound[a], a] <: SelfRead[s, a] & SelfWrite[s, a],
      SelfRead[+_[a] <: BoundRead[a], _],
      SelfWrite[+_[a] <: BoundWrite[a], _],
      Bound[a] <: BoundRead[a] & BoundWrite[a],
      BoundRead[_],
      BoundWrite[_]
  ](using
      self: CollectionOperation[Self, SelfRead, SelfWrite, Bound, BoundRead, BoundWrite]
  ): CollectionOperation[Self, SelfRead, SelfWrite, Bound, BoundRead, BoundWrite] = self

  given InvariantK6[CollectionOperation]:
    extension [
        Self[+s[a] <: Bound[a], a] <: SelfRead[s, a] & SelfWrite[s, a],
        SelfRead[+_[a] <: BoundRead[a], _],
        SelfWrite[+_[a] <: BoundWrite[a], _],
        Bound[a] <: BoundRead[a] & BoundWrite[a],
        BoundRead[_],
        BoundWrite[_]
    ](fa: CollectionOperation[Self, SelfRead, SelfWrite, Bound, BoundRead, BoundWrite])
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
      ): CollectionOperation[SelfK, SelfReadK, SelfWriteK, Bound, BoundRead, BoundWrite] =
        fa.mapK(fK)(fKR)(fKW)
