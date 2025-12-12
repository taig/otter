package io.taig.otter.base

import cats.data.Chain
import io.taig.otter.Record
import io.taig.otter.Reference
import cats.Invariant
import cats.Contravariant
import cats.Functor

sealed abstract class RecordBase[+S[_], A] extends RecordBase.Read[S, A], RecordBase.Write[S, A]:
  final def imap[B](f: A => B)(g: B => A): RecordBase[S, B] = ???

  // final def zip[S1[+_[_], _], U[a] >: T[a], B](schema: RecordBase[S1, U, B]): RecordBase[S1, U, (A, B)] = ???

object RecordBase:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def fields: Chain[Reference[S, ?]]

    final def map[B](f: A => B): RecordBase.Read[S, B] = ???

  object Read:
    given [S[_]]: Functor[RecordBase.Read[S, *]] with
      override def map[A, B](fa: RecordBase.Read[S, A])(f: A => B): RecordBase.Read[S, B] = fa.map(f)
    // given [S[+_[_], _], T[_]]: Record.Read[[t[_], a] =>> RecordBase.Read[S, t, a], S, T] = ???

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    // def fields: Chain[Reference[S[T, *], ?]]

    final def contramap[B](f: B => A): RecordBase.Write[S, B] = ???

  object Write:
    given [S[_]]: Contravariant[RecordBase.Write[S, *]] with
      override def contramap[A, B](fa: RecordBase.Write[S, A])(f: B => A): RecordBase.Write[S, B] =
        fa.contramap(f)

    // given [S[+_[_], _], T[_]]: Record.Write[[t[_], a] =>> RecordBase.Write[S, t, a], S, T] = ???

  case object Empty extends RecordBase[Nothing, Unit]:
    override def fields: Chain[Nothing] = Chain.empty

  final case class Root[S[_], A](field: Reference[S, A]) extends RecordBase[S, A]:
    override def fields: Chain[Reference[S, ?]] = Chain.one(field)

  given [S[_]]: Invariant[RecordBase[S, *]] with
    override def imap[A, B](fa: RecordBase[S, A])(f: A => B)(g: B => A): RecordBase[S, B] = fa.imap(f)(g)

  given [S[+_[a] <: T[a], _], T[_]]: Record[[s[a] <: T[a], a] =>> RecordBase[S[s, *], a], S, T] with
    override def apply[I[a] <: T[a], A](field: Reference[S[I, *], A]): RecordBase[S[I, *], A] = ???

    override def empty: RecordBase[Nothing, Unit] = ???

    extension [I[a] <: T[a], A](fia: RecordBase[S[I, *], A])
      override def fields: Chain[Reference[S[I, *], ?]] = ???

      override def zip[J[a] >: I[a] <: T[a], B](schema: RecordBase[S[J, *], B]): RecordBase[S[J, *], (A, B)] = ???
