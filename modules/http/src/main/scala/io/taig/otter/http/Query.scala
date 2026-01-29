package io.taig.otter.http

import io.taig.otter as Self
import cats.Functor
import cats.Contravariant
import cats.Invariant
import Self.Reference
import cats.Eval
import Self.http.operation.QueryOperation

abstract class Query[+F[_], A] extends Query.Read[F, A], Query.Write[F, A]:
  final def imap[B](f: A => B)(g: B => A): Query[F, B] = Query.Modify(self = this, f, g)

  final override def optional: Query[F, Option[A]] = Query.Optional(self = this)

  final def optional(default: Eval[A]): Query[F, A] = Query.Default(self = this, value = default)

  override def parameter: Reference[F, ?]

object Query:
  trait Read[+F[_], +A]:
    final def map[B](f: A => B): Query.Read[F, B] = Read.Modify(self = this, f)

    def name: String

    def optional: Query.Read[F, Option[A]] = Read.Optional(self = this)

    def optional[A1 >: A](default: Eval[A1]): Query.Read[F, A1] = Read.Default(self = this, value = default)

    def parameter: Reference[F, ?]

  object Read:
    final case class Default[F[_], A](self: Query.Read[F, A], value: Eval[A]) extends Query.Read[F, A]:
      export self.{name, parameter}

    final case class Modify[F[_], A, B](self: Query.Read[F, A], f: A => B) extends Query.Read[F, B]:
      export self.{name, parameter}

    final case class Optional[F[_], A](self: Query.Read[F, A]) extends Query.Read[F, Option[A]]:
      export self.{name, parameter}

    given [F[_]] => Functor[Query.Read[F, *]]:
      override def map[A, B](query: Query.Read[F, A])(f: A => B): Query.Read[F, B] = query.map(f)

    given [F[_]] => QueryOperation.Read[Query.Read[F, *], F]:
      override def lift[A](name: String, parameter: Reference[F, A]): Query.Read[F, A] =
        Root(name, parameter)

  trait Write[+F[_], -A]:
    final def contramap[B](f: B => A): Query.Write[F, B] = Write.Modify(self = this, f)

    def name: String

    def optional: Query.Write[F, Option[A]] = Write.Optional(self = this)

    def parameter: Reference[F, ?]

  object Write:
    final case class Modify[F[_], A, B](self: Query.Write[F, A], f: B => A) extends Query.Write[F, B]:
      export self.{name, parameter}

    final case class Optional[F[_], A](self: Query.Write[F, A]) extends Query.Write[F, Option[A]]:
      export self.{name, parameter}

    final case class Root[F[_], A](name: String, parameter: Reference[F, A]) extends Query.Write[F, A]

    given [F[_]] => Contravariant[Query.Write[F, *]]:
      override def contramap[A, B](query: Query.Write[F, A])(f: B => A): Query.Write[F, B] = query.contramap(f)

    given [F[_]] => QueryOperation.Write[Query.Write[F, *], F]:
      override def lift[A](name: String, parameter: Reference[F, A]): Query.Write[F, A] =
        Root(name, parameter)

  final case class Default[F[_], A](self: Query[F, A], value: Eval[A]) extends Query[F, A]:
    export self.{name, parameter}

  final case class Modify[F[_], A, B](self: Query[F, A], f: A => B, g: B => A) extends Query[F, B]:
    export self.{name, parameter}

  final case class Optional[F[_], A](self: Query[F, A]) extends Query[F, Option[A]]:
    export self.{name, parameter}

  final case class Root[F[_], A](name: String, parameter: Reference[F, A]) extends Query[F, A]

  given [F[_]] => Invariant[Query[F, *]]:
    override def imap[A, B](fa: Query[F, A])(f: A => B)(g: B => A): Query[F, B] = fa.imap(f)(g)

  given [F[_]] => QueryOperation[Query[F, *], F]:
    override def lift[A](name: String, parameter: Reference[F, A]): Query[F, A] =
      Root(name, parameter)
