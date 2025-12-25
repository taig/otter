package io.taig.otter

import io.taig.otter.operation.TupleOperation
import scala.annotation.targetName
import cats.data.Chain
import io.taig.otter as Self
import cats.Invariant
import cats.Contravariant
import cats.Functor

sealed abstract class Tuple[+S[_], A] extends Tuple.Read[S, A], Tuple.Write[S, A]:
  final def imap[B](f: A => B)(g: B => A): Tuple[S, B] = Tuple.Modify(self = this, f, g)

  final def zip[S1[a] >: S[a], B](schema: Tuple[S1, B]): Tuple[S1, (A, B)] = Tuple.Zip(left = this, right = schema)

object Tuple:
  sealed trait Read[+S[_], +A]:
    final def map[B](f: A => B): Tuple.Read[S, B] = Read.Modify(self = this, f)

    def schemas: Chain[Reference[S, ?]]

    final def zip[S1[a] >: S[a], B](schema: Tuple.Read[S1, B]): Tuple.Read[S1, (A, B)] =
      Read.Zip(left = this, right = schema)

  object Read:
    final case class Modify[S[_], A, B](self: Tuple.Read[S, A], f: A => B) extends Tuple.Read[S, B]:
      export self.schemas

    final case class Zip[S[_], A, B](left: Tuple.Read[S, A], right: Tuple.Read[S, B]) extends Tuple.Read[S, (A, B)]:
      override def schemas: Chain[Self.Reference[S, ?]] = left.schemas ++ right.schemas

    given [S[_]] => Functor[Tuple.Read[S, *]]:
      override def map[A, B](fa: Self.Tuple.Read[S, A])(f: A => B): Self.Tuple.Read[S, B] =
        fa.map(f)

    given [Schema[+_[a] <: Bound[a], a] <: Bound[a], Bound[_]] => TupleOperation.Read[Tuple.Read, Schema, Bound]:
      @targetName("applyRead")
      override def apply[S[+s[a] <: Bound[a], a] <: Schema[s, a], T[a] <: Bound[a], A](
          schema: Reference[S[T, *], A]
      ): Tuple.Read[S[T, *], A] = Root(schema)

      override def empty: Tuple.Read[Nothing, Unit] = Empty

      extension [F[a] <: Bound[a], A](self: Tuple.Read[F, A])
        @targetName("zipRead")
        override def zip[G[a] >: F[a] <: Bound[a], B](schema: Tuple.Read[G, B]): Tuple.Read[G, (A, B)] =
          self.zip(schema)

  sealed trait Write[+S[_], -A]:
    final def contramap[B](f: B => A): Tuple.Write[S, B] = Write.Modify(self = this, f)

    def schemas: Chain[Reference[S, ?]]

    final def zip[S1[a] >: S[a], B](schema: Tuple.Write[S1, B]): Tuple.Write[S1, (A, B)] =
      Write.Zip(left = this, right = schema)

  object Write:
    final case class Modify[S[_], A, B](self: Tuple.Write[S, A], f: B => A) extends Tuple.Write[S, B]:
      export self.schemas

    final case class Zip[S[_], A, B](left: Tuple.Write[S, A], right: Tuple.Write[S, B]) extends Tuple.Write[S, (A, B)]:
      override def schemas: Chain[Self.Reference[S, ?]] = left.schemas ++ right.schemas

    given [S[_]] => Contravariant[Tuple.Write[S, *]]:
      override def contramap[A, B](fa: Self.Tuple.Write[S, A])(f: B => A): Self.Tuple.Write[S, B] =
        fa.contramap(f)

    given [Schema[+_[a] <: Bound[a], a] <: Bound[a], Bound[_]] => TupleOperation.Write[Tuple.Write, Schema, Bound]:
      @targetName("applyWrite")
      override def apply[S[+s[a] <: Bound[a], a] <: Schema[s, a], T[a] <: Bound[a], A](
          schema: Reference[S[T, *], A]
      ): Tuple.Write[S[T, *], A] = Root(schema)

      override def empty: Tuple.Write[Nothing, Unit] = Empty

      extension [F[a] <: Bound[a], A](self: Tuple.Write[F, A])
        @targetName("zipWrite")
        override def zip[G[a] >: F[a] <: Bound[a], B](schema: Tuple.Write[G, B]): Tuple.Write[G, (A, B)] =
          self.zip(schema)

  case object Empty extends Tuple[Nothing, Unit]:
    override def schemas: Chain[Nothing] = Chain.empty

  final case class Modify[S[_], A, B](self: Tuple[S, A], f: A => B, g: B => A) extends Tuple[S, B]:
    export self.schemas

  final case class Root[S[_], A](schema: Reference[S, A]) extends Tuple[S, A]:
    override def schemas: Chain[Reference[S, A]] = Chain.one(schema)

  final case class Zip[S[_], A, B](left: Tuple[S, A], right: Tuple[S, B]) extends Tuple[S, (A, B)]:
    override def schemas: Chain[Reference[S, ?]] = left.schemas ++ right.schemas

  given [S[_]] => Invariant[Tuple[S, *]]:
    override def imap[A, B](self: Self.Tuple[S, A])(f: A => B)(g: B => A): Self.Tuple[S, B] = self.imap(f)(g)

  given [
      Schema[+_[a] <: Bound[a], a] <: Bound[a],
      SchemaRead[+_[a] <: BoundRead[a], a] <: BoundRead[a],
      SchemaWrite[+_[a] <: BoundWrite[a], a] <: BoundWrite[a],
      Bound[a] <: BoundRead[a] & BoundWrite[a],
      BoundRead[+_],
      BoundWrite[-_]
  ] => TupleOperation[
        Tuple,
        Tuple.Read,
        Tuple.Write,
        Schema,
        SchemaRead,
        SchemaWrite,
        Bound,
        BoundRead,
        BoundWrite
      ]:
    override def apply[S[+_[a] <: Bound[a], a] <: Bound[a], T[a] <: Bound[a], A](
        schema: Reference[S[T, *], A]
    ): Tuple[S[T, *], A] = Root(schema)

    override def empty: Tuple[Nothing, Unit] = Empty

    extension [F[a] <: Bound[a], A](self: Tuple[F, A])
      override def zip[G[a] >: F[a] <: Bound[a], B](schema: Tuple[G, B]): Tuple[G, (A, B)] =
        self.zip(schema)

    extension [F[a] <: BoundRead[a], A](self: Tuple.Read[F, A])
      @targetName("zipRead")
      override def zip[G[a] >: F[a] <: BoundRead[a], B](schema: Tuple.Read[G, B]): Tuple.Read[G, (A, B)] =
        self.zip(schema)

    extension [F[a] <: BoundWrite[a], A](self: Tuple.Write[F, A])
      @targetName("zipWrite")
      override def zip[G[a] >: F[a] <: BoundWrite[a], B](schema: Tuple.Write[G, B]): Tuple.Write[G, (A, B)] =
        self.zip(schema)
