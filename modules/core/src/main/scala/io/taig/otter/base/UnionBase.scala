package io.taig.otter.base

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.data.NonEmptyChain
import io.taig.otter.Union
import io.taig.otter.Reference

sealed abstract class UnionBase[+S[_], A] extends UnionBase.Read[S, A], UnionBase.Write[S, A]:
  final def orElse[T[_], B](that: UnionBase[T, B]): UnionBase[[a] =>> S[a] | T[a], Either[A, B]] =
    UnionBase.OrElse(left = this, right = that)

  final def imap[T](f: A => T)(g: T => A): UnionBase[S, T] = UnionBase.Modify(self = this, f, g)

object UnionBase:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def branches: NonEmptyChain[Reference[S, ?]]

    final def map[T](f: A => T): UnionBase.Read[S, T] = Read.Modify(self = this, f)

    final def orElse[T[_], B](that: UnionBase.Read[T, B]): UnionBase.Read[[a] =>> S[a] | T[a], Either[A, B]] =
      Read.OrElse(left = this, right = that)

  object Read:
    final case class OrElse[S[_], T[_], A, B](left: UnionBase.Read[S, A], right: UnionBase.Read[T, B])
        extends UnionBase.Read[[a] =>> S[a] | T[a], Either[A, B]]:
      override def branches: NonEmptyChain[Reference[[a] =>> S[a] | T[a], ?]] = left.branches ++ right.branches

    final case class Modify[S[_], A, B](self: UnionBase.Read[S, A], f: A => B) extends UnionBase.Read[S, B]:
      export self.branches

    given [S[_]]: Functor[UnionBase.Read[S, *]] with
      def map[A, B](fa: UnionBase.Read[S, A])(f: A => B): UnionBase.Read[S, B] = fa.map(f)

    given [S[_]]: Union.Read[UnionBase.Read, S] with
      // override def apply[T[a] <: S[a], A](branch: Branch[T, A]): UnionBase.Read[T, A] = UnionBase.Read.Root(branch)

      extension [A](fha: UnionBase.Read[S, A]) override def branches: NonEmptyChain[Reference[S, ?]] = fha.branches

      extension [T[a] <: S[a], A](fia: UnionBase.Read[T, A])
        override def orElse[U[a] <: S[a], B](
            schema: UnionBase.Read[U, B]
        ): UnionBase.Read[[a] =>> T[a] | U[a], Either[A, B]] = fia.orElse(schema)

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def branches: NonEmptyChain[Reference[S, ?]]

    final def contramap[T](f: T => A): UnionBase.Write[S, T] = Write.Modify(self = this, f)

    final def orElse[T[_], B](that: UnionBase.Write[T, B]): UnionBase.Write[[a] =>> S[a] | T[a], Either[A, B]] =
      Write.OrElse(left = this, right = that)

  object Write:
    final case class OrElse[S[_], T[_], A, B](left: UnionBase.Write[S, A], right: UnionBase.Write[T, B])
        extends UnionBase.Write[[a] =>> S[a] | T[a], Either[A, B]]:
      override def branches: NonEmptyChain[Reference[[a] =>> S[a] | T[a], ?]] = left.branches ++ right.branches

    final case class Modify[S[_], A, B](self: UnionBase.Write[S, A], f: B => A) extends UnionBase.Write[S, B]:
      export self.branches

    given [S[_]]: Contravariant[UnionBase.Write[S, *]] with
      def contramap[A, B](fa: UnionBase.Write[S, A])(f: B => A): UnionBase.Write[S, B] = fa.contramap(f)

    given [S[_]]: Union.Write[UnionBase.Write, S] with
      // override def apply[T[a] <: S[a], A](branch: Branch[T, A]): UnionBase.Write[T, A] = UnionBase.Write.Root(branch)

      extension [A](self: UnionBase.Write[S, A]) override def branches: NonEmptyChain[Reference[S, ?]] = self.branches

      extension [T[a] <: S[a], A](self: UnionBase.Write[T, A])
        override def orElse[U[a] <: S[a], B](
            schema: UnionBase.Write[U, B]
        ): UnionBase.Write[[a] =>> T[a] | U[a], Either[A, B]] = self.orElse(schema)

  final case class Root[S[_], A](branch: Reference[S, A]) extends UnionBase[S, A]:
    override def branches: NonEmptyChain[Reference[S, ?]] = NonEmptyChain.one(branch)

  final case class OrElse[S[_], T[_], A, B](left: UnionBase[S, A], right: UnionBase[T, B])
      extends UnionBase[[a] =>> S[a] | T[a], Either[A, B]]:
    override def branches: NonEmptyChain[Reference[[a] =>> S[a] | T[a], ?]] = left.branches ++ right.branches

  final case class Modify[S[_], A, B](self: UnionBase[S, A], f: A => B, g: B => A) extends UnionBase[S, B]:
    export self.branches

  given [S[_]]: Invariant[UnionBase[S, *]] with
    def imap[A, B](fa: UnionBase[S, A])(f: A => B)(g: B => A): UnionBase[S, B] = fa.imap(f)(g)

  given [S[_]]: Union[UnionBase, S] with
    // override def apply[T[a] <: S[a], A](branch: Branch[T, A]): UnionBase[T, A] = Root(branch)

    extension [A](self: UnionBase[S, A]) override def branches: NonEmptyChain[Reference[S, ?]] = self.branches

    extension [T[a] <: S[a], A](self: UnionBase[T, A])
      override def orElse[U[a] <: S[a], B](schema: UnionBase[U, B]): UnionBase[[a] =>> T[a] | U[a], Either[A, B]] =
        OrElse(left = self, right = schema)
