package io.taig.otter.base

import cats.data.Chain
import io.taig.otter.Record
import io.taig.otter.Reference
import cats.Invariant
import cats.Contravariant
import cats.Functor

sealed abstract class RecordBase[+S[+_[a] <: T[a], _], +T[_], A] extends RecordBase.Read[S, T, A], RecordBase.Write[S, T, A]:
  final def imap[B](f: A => B)(g: B => A): RecordBase[S, T, B] = ???

  // final def zip[S1[+_[_], _], U[a] >: T[a], B](schema: RecordBase[S1, U, B]): RecordBase[S1, U, (A, B)] = ???

object RecordBase:
  sealed trait Read[+S[+_[a] <: T[a], _], +T[_], +A] extends Product, Serializable:
    // def fields: Chain[Reference[S[T, *], ?]]

    final def map[B](f: A => B): RecordBase.Read[S, T, B] = ???

  object Read:
    given [S[+_[_], _], T[_]]: Functor[RecordBase.Read[S, T, *]] with
      override def map[A, B](fa: RecordBase.Read[S, T, A])(f: A => B): RecordBase.Read[S, T, B] = fa.map(f)

    // given [S[+_[_], _], T[_]]: Record.Read[[t[_], a] =>> RecordBase.Read[S, t, a], S, T] = ???

  sealed trait Write[+S[+_[a] <: T[a], _], +T[_], -A] extends Product, Serializable:
    // def fields: Chain[Reference[S[T, *], ?]]

    final def contramap[B](f: B => A): RecordBase.Write[S, T, B] = ???

  object Write:
    given [S[+_[_], _], T[_]]: Contravariant[RecordBase.Write[S, T, *]] with
      override def contramap[A, B](fa: RecordBase.Write[S, T, A])(f: B => A): RecordBase.Write[S, T, B] =
        fa.contramap(f)

    // given [S[+_[_], _], T[_]]: Record.Write[[t[_], a] =>> RecordBase.Write[S, t, a], S, T] = ???

  case object Empty extends RecordBase[Nothing, Nothing, Unit]
    // override def fields: Chain[Nothing] = Chain.empty

  final case class Root[S[+_[_], _], T[_], A](field: Reference[S[T, *], A]) extends RecordBase[S, T, A]
    // override def fields: Chain[Reference[S[T, *], ?]] = Chain.one(field)

  given [S[+_[a] <: T[a], _], T[_]]: Invariant[RecordBase[S, T, *]] with
    override def imap[A, B](fa: RecordBase[S, T, A])(f: A => B)(g: B => A): RecordBase[S, T, B] = fa.imap(f)(g)

  // given [S[+_[a], _], T[_]]: Record[[t[a] <: T[a], a] =>> RecordBase[S, t, a], S, T] with
  //   override def apply[U[a] <: T[a], A](field: Reference[S[U, *], A]): RecordBase[S, U, A] = Root(field)

  //   override def empty: RecordBase[S, Nothing, Unit] = ???

  //   extension [U[a] <: T[a], A](self: RecordBase[S, U, A])
  //     override def fields: Chain[Reference[S[U, *], ?]] = self.fields

  //     override def zip[V[a] >: U[a] <: T[a], B](schema: RecordBase[S, V, B]): RecordBase[S, V, (A, B)] =
  //       ???
