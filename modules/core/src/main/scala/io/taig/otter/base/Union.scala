package io.taig.otter.base

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.data.NonEmptyChain
import io.taig.otter as Self
import io.taig.otter.Branch

sealed abstract class Union[+S[_], A] extends Union.Read[S, A], Union.Write[S, A]:
  final def imap[T](f: A => T)(g: T => A): Union[S, T] = Union.Modify(self = this, f, g)

object Union:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def branches: NonEmptyChain[Branch[S, ?]]

    final def map[T](f: A => T): Union.Read[S, T] = Read.Modify(self = this, f)

    final def orElse[T[_], B](that: Union.Read[T, B]): Union.Read[[a] =>> S[a] | T[a], Either[A, B]] =
      Read.OrElse(self = this, that)

  object Read:
    final case class Root[S[_], A](branch: Branch[S, A]) extends Union.Read[S, A]:
      override def branches: NonEmptyChain[Branch[S, ?]] = NonEmptyChain.one(branch)

    final case class OrElse[S[_], T[_], A, B](self: Union.Read[S, A], that: Union.Read[T, B])
        extends Union.Read[[a] =>> S[a] | T[a], Either[A, B]]:
      override def branches: NonEmptyChain[Branch[[a] =>> S[a] | T[a], ?]] = self.branches ++ that.branches

    final case class Modify[S[_], A, B](self: Union.Read[S, A], f: A => B) extends Union.Read[S, B]:
      export self.branches

    given [S[_]]: Functor[Union.Read[S, *]] with
      def map[A, B](fa: Union.Read[S, A])(f: A => B): Union.Read[S, B] = fa.map(f)

    given [S[_]]: Self.Union.Read[Union.Read, S] with
      override def union[T[a] <: S[a], A](branch: Branch[T, A]): Union.Read[T, A] = Root(branch)

      override def orElse[T[a] <: S[a], A, B](
          left: Union.Read[T, A],
          right: Union.Read[T, B]
      ): Union.Read[T, Either[A, B]] = OrElse(left, right)

      override def branches[T[a] <: S[a], A](self: Union.Read[T, A]): NonEmptyChain[Branch[T, ?]] = self.branches

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def branches: NonEmptyChain[Branch[S, ?]]

    final def contramap[T](f: T => A): Union.Write[S, T] = Write.Modify(self = this, f)

    final def orElse[T[_], B](that: Union.Write[T, B]): Union.Write[[a] =>> S[a] | T[a], Either[A, B]] =
      Write.OrElse(self = this, that)

  object Write:
    final case class Root[S[_], A](branch: Branch[S, A]) extends Union.Write[S, A]:
      override def branches: NonEmptyChain[Branch[S, ?]] = NonEmptyChain.one(branch)

    final case class OrElse[S[_], T[_], A, B](self: Union.Write[S, A], that: Union.Write[T, B])
        extends Union.Write[[a] =>> S[a] | T[a], Either[A, B]]:
      override def branches: NonEmptyChain[Branch[[a] =>> S[a] | T[a], ?]] = self.branches ++ that.branches

    final case class Modify[S[_], A, B](self: Union.Write[S, A], f: B => A) extends Union.Write[S, B]:
      export self.branches

    given [S[_]]: Contravariant[Union.Write[S, *]] with
      def contramap[A, B](fa: Union.Write[S, A])(f: B => A): Union.Write[S, B] = fa.contramap(f)

    given [S[_]]: Self.Union.Write[Union.Write, S] with
      override def union[T[a] <: S[a], A](branch: Branch[T, A]): Union.Write[T, A] = Root(branch)

      override def orElse[T[a] <: S[a], A, B](
          left: Union.Write[T, A],
          right: Union.Write[T, B]
      ): Union.Write[T, Either[A, B]] = OrElse(left, right)

      override def branches[T[a] <: S[a], A](self: Union.Write[T, A]): NonEmptyChain[Branch[T, ?]] = self.branches

  final case class Root[S[_], A](branch: Branch[S, A]) extends Union[S, A]:
    override def branches: NonEmptyChain[Branch[S, ?]] = NonEmptyChain.one(branch)

  final case class OrElse[S[_], T[_], A, B](self: Union[S, A], that: Union[T, B])
      extends Union[[a] =>> S[a] | T[a], Either[A, B]]:
    override def branches: NonEmptyChain[Branch[[a] =>> S[a] | T[a], ?]] = self.branches ++ that.branches

  final case class Modify[S[_], A, B](self: Union[S, A], f: A => B, g: B => A) extends Union[S, B]:
    export self.branches

  given [S[_]]: Invariant[Union[S, *]] with
    def imap[A, B](fa: Union[S, A])(f: A => B)(g: B => A): Union[S, B] = fa.imap(f)(g)

  given [S[_]]: Self.Union[Union, S] with
    override def union[T[a] <: S[a], A](branch: Branch[T, A]): Union[T, A] = Root(branch)

    override def orElse[T[a] <: S[a], A, B](
        left: Union[T, A],
        right: Union[T, B]
    ): Union[T, Either[A, B]] = OrElse(left, right)

    override def branches[T[a] <: S[a], A](self: Union[T, A]): NonEmptyChain[Branch[T, ?]] = self.branches
