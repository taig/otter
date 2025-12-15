package io.taig.otter.base

import cats.Contravariant
import cats.Functor
import cats.Invariant
import io.taig.otter.Branch
import io.taig.otter.Reference

sealed abstract class BranchBase[+S[_], A] extends BranchBase.Read[S, A], BranchBase.Write[S, A]:
  final def imap[T](f: A => T)(g: T => A): BranchBase[S, T] = BranchBase.Modify(self = this, f, g)

object BranchBase:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def name: String

    def schema: Reference[S, ?]

    final def map[T](f: A => T): BranchBase.Read[S, T] = Read.Modify(self = this, f)

  object Read:
    final case class Modify[S[_], A, B](self: BranchBase.Read[S, A], f: A => B) extends Read[S, B]:
      export self.{name, schema}

    given [S[_]]: Functor[BranchBase.Read[S, *]] with
      def map[A, B](fa: BranchBase.Read[S, A])(f: A => B): BranchBase.Read[S, B] = fa.map(f)

    given [S[_]]: Branch.Read[BranchBase.Read, S] with
      override def apply[T[a] <: S[a], A](name: String, schema: Reference[T, A]): BranchBase.Read[T, A] =
        BranchBase.Root(name, schema)

      extension [A](self: BranchBase.Read[S, A]) override def name: String = self.name

      extension [T[a] <: S[a], A](self: BranchBase.Read[T, A]) override def schema: Reference[T, ?] = self.schema

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def name: String

    def schema: Reference[S, ?]

    final def contramap[T](f: T => A): BranchBase.Write[S, T] = Write.Modify(self = this, f)

  object Write:
    final case class Modify[S[_], A, B](self: BranchBase.Write[S, A], f: B => A) extends Write[S, B]:
      export self.{name, schema}

    given [S[_]]: Contravariant[BranchBase.Write[S, *]] with
      def contramap[A, B](fa: BranchBase.Write[S, A])(f: B => A): BranchBase.Write[S, B] = fa.contramap(f)

    given [S[_]]: Branch.Write[BranchBase.Write, S] with
      override def apply[T[a] <: S[a], A](name: String, schema: Reference[T, A]): BranchBase.Write[T, A] =
        BranchBase.Root(name, schema)

      extension [A](self: BranchBase.Write[S, A]) override def name: String = self.name

      extension [T[a] <: S[a], A](self: BranchBase.Write[T, A]) override def schema: Reference[T, ?] = self.schema

  final case class Modify[S[_], A, B](self: BranchBase[S, A], f: A => B, g: B => A) extends BranchBase[S, B]:
    export self.{name, schema}

  final case class Root[S[_], A](name: String, schema: Reference[S, A]) extends BranchBase[S, A]

  given [S[_]]: Invariant[BranchBase[S, *]] with
    def imap[A, B](fa: BranchBase[S, A])(f: A => B)(g: B => A): BranchBase[S, B] = fa.imap(f)(g)

  given [S[_]]: Branch[BranchBase, S] with
    override def apply[T[a] <: S[a], A](name: String, schema: Reference[T, A]): BranchBase[T, A] =
      BranchBase.Root(name, schema)

    extension [A](self: BranchBase[S, A]) override def name: String = self.name
    extension [T[a] <: S[a], A](self: BranchBase[T, A]) override def schema: Reference[T, ?] = self.schema
