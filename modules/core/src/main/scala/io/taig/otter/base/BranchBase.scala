package io.taig.otter.base

import cats.Contravariant
import cats.Functor
import cats.Invariant
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

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def name: String

    def schema: Reference[S, ?]

    final def contramap[T](f: T => A): BranchBase.Write[S, T] = Write.Modify(self = this, f)

  object Write:
    final case class Modify[S[_], A, B](self: BranchBase.Write[S, A], f: B => A) extends Write[S, B]:
      export self.{name, schema}

    given [S[_]]: Contravariant[BranchBase.Write[S, *]] with
      def contramap[A, B](fa: BranchBase.Write[S, A])(f: B => A): BranchBase.Write[S, B] = fa.contramap(f)

  final case class Modify[S[_], A, B](self: BranchBase[S, A], f: A => B, g: B => A) extends BranchBase[S, B]:
    export self.{name, schema}

  final case class Root[S[_], A](name: String, schema: Reference[S, A]) extends BranchBase[S, A]

  given [S[_]]: Invariant[BranchBase[S, *]] with
    def imap[A, B](fa: BranchBase[S, A])(f: A => B)(g: B => A): BranchBase[S, B] = fa.imap(f)(g)
