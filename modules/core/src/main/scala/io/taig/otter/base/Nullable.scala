package io.taig.otter.base

import io.taig.otter.Reference
import cats.Eval
import cats.Contravariant
import cats.Functor
import cats.Invariant

sealed abstract class Nullable[+S[_], A] extends Nullable.Read[S, A], Nullable.Write[S, A]:
  final def imap[T](f: A => T)(g: T => A): Nullable[S, T] = Nullable.Modify(self = this, f, g)

object Nullable:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def map[T](f: A => T): Nullable.Read[S, T] = Read.Modify(this, f)

  object Read:
    final case class Default[S[_], A](schema: Reference[S, A], default: Eval[A]) extends Nullable.Read[S, A]

    final case class Modify[S[_], A, B](self: Nullable.Read[S, A], f: A => B) extends Nullable.Read[S, B]:
      export self.schema

    final case class Optional[S[_], A](schema: Reference[S, A]) extends Nullable.Read[S, Option[A]]

    given [S[_]]: Functor[Nullable.Read[S, *]] with
      override def map[A, B](fa: Nullable.Read[S, A])(f: A => B): Nullable.Read[S, B] = fa.map(f)

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def contramap[T](f: T => A): Nullable.Write[S, T] = Write.Modify(this, f)

  object Write:
    final case class Modify[S[_], A, B](self: Nullable.Write[S, A], f: B => A) extends Nullable.Write[S, B]:
      export self.schema

    final case class Optional[S[_], A](self: Nullable.Write[S, A]) extends Nullable.Write[S, Option[A]]:
      export self.schema

    given [S[_]]: Contravariant[Nullable.Write[S, *]] with
      override def contramap[A, B](fa: Nullable.Write[S, A])(f: B => A): Nullable.Write[S, B] = fa.contramap(f)

  final case class Default[S[_], A](self: Nullable.Read[S, A], default: Eval[A]) extends Nullable[S, A]:
    export self.schema

  final case class Modify[S[_], A, B](self: Nullable.Write[S, A], f: A => B, g: B => A) extends Nullable[S, B]:
    export self.schema

  final case class Optional[S[_], A](self: Nullable.Write[S, A]) extends Nullable[S, Option[A]]:
    export self.schema

  given [S[_]]: Invariant[Nullable[S, *]] with
    override def imap[A, B](fa: Nullable[S, A])(f: A => B)(g: B => A): Nullable[S, B] = fa.imap(f)(g)
