package io.taig.otter.base

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.data.NonEmptyChain
import io.taig.otter.Union
import io.taig.otter.Reference
import cats.data.NonEmptyChainImpl.Type

sealed abstract class UnionBase[+S[_], A] extends UnionBase.Read[S, A], UnionBase.Write[S, A]:
  final def orElse[S1[a] >: S[a], B](that: UnionBase[S1, B]): UnionBase[S1, Either[A, B]] =
    UnionBase.OrElse(left = this, right = that)

  final def imap[T](f: A => T)(g: T => A): UnionBase[S, T] = UnionBase.Modify(self = this, f, g)

object UnionBase:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def branches: NonEmptyChain[Reference[S, ?]]

    final def map[T](f: A => T): UnionBase.Read[S, T] = Read.Modify(self = this, f)

    final def orElse[S1[a] >: S[a], B](that: UnionBase.Read[S1, B]): UnionBase.Read[S1, Either[A, B]] =
      Read.OrElse(left = this, right = that)

  object Read:
    final case class OrElse[S[_], A, B](left: UnionBase.Read[S, A], right: UnionBase.Read[S, B])
        extends UnionBase.Read[S, Either[A, B]]:
      override def branches: NonEmptyChain[Reference[S, ?]] = left.branches ++ right.branches

    final case class Modify[S[_], A, B](self: UnionBase.Read[S, A], f: A => B) extends UnionBase.Read[S, B]:
      export self.branches

    given [S[_]]: Functor[UnionBase.Read[S, *]] with
      def map[A, B](fa: UnionBase.Read[S, A])(f: A => B): UnionBase.Read[S, B] = fa.map(f)

    given [S[+_[a] <: T[a], _], T[_]]: Union.Read[[s[a] <: T[a], a] =>> UnionBase.Read[S[s, *], a], S, T] = ???

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def branches: NonEmptyChain[Reference[S, ?]]

    final def contramap[T](f: T => A): UnionBase.Write[S, T] = Write.Modify(self = this, f)

    final def orElse[S1[a] >: S[a], B](that: UnionBase.Write[S1, B]): UnionBase.Write[S1, Either[A, B]] =
      Write.OrElse(left = this, right = that)

  object Write:
    final case class OrElse[S[_], A, B](left: UnionBase.Write[S, A], right: UnionBase.Write[S, B])
        extends UnionBase.Write[S, Either[A, B]]:
      override def branches: NonEmptyChain[Reference[S, ?]] = left.branches ++ right.branches

    final case class Modify[S[_], A, B](self: UnionBase.Write[S, A], f: B => A) extends UnionBase.Write[S, B]:
      export self.branches

    given [S[_]]: Contravariant[UnionBase.Write[S, *]] with
      def contramap[A, B](fa: UnionBase.Write[S, A])(f: B => A): UnionBase.Write[S, B] = fa.contramap(f)

    given [S[+_[a] <: T[a], _], T[_]]: Union.Write[[s[a] <: T[a], a] =>> UnionBase.Write[S[s, *], a], S, T] = ???

  final case class Root[S[_], A](branch: Reference[S, A]) extends UnionBase[S, A]:
    override def branches: NonEmptyChain[Reference[S, ?]] = NonEmptyChain.one(branch)

  final case class OrElse[S[_], A, B](left: UnionBase[S, A], right: UnionBase[S, B]) extends UnionBase[S, Either[A, B]]:
    override def branches: NonEmptyChain[Reference[S, ?]] = left.branches ++ right.branches

  final case class Modify[S[_], A, B](self: UnionBase[S, A], f: A => B, g: B => A) extends UnionBase[S, B]:
    export self.branches

  given [S[_]]: Invariant[UnionBase[S, *]] with
    def imap[A, B](fa: UnionBase[S, A])(f: A => B)(g: B => A): UnionBase[S, B] = fa.imap(f)(g)

  given [S[+_[a] <: T[a], _], T[_]]: Union[[s[a] <: T[a], a] =>> UnionBase[S[s, *], a], S, T] with
    override def apply[U[a] <: T[a], A](field: Reference[S[U, *], A]): UnionBase[S[U, *], A] = Root(field)

    extension [A](fha: UnionBase[S[T, *], A]) override def branches: NonEmptyChain[Reference[S[T, *], ?]] = fha.branches

    extension [U[a] <: T[a], A](self: UnionBase[S[U, *], A])
      override def orElse[J[a] >: U[a] <: T[a], B](schema: UnionBase[S[J, *], B]): UnionBase[S[J, *], Either[A, B]] =
        self.orElse(schema)
