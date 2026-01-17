package io.taig.otter

import cats.Contravariant
import cats.Functor
import cats.Invariant
import io.taig.otter.operation.BranchOperation

sealed abstract class Branch[+S[_], A] extends Branch.Read[S, A], Branch.Write[S, A]:
  final def imap[B](f: A => B)(g: B => A): Branch[S, B] = Branch.Modify(self = this, f, g)

  def mapK[G[_]](fK: [A] => S[A] => G[A]): Branch[G, A]

object Branch:
  sealed trait Read[+S[_], +A]:
    def name: String

    def schema: Reference[S, ?]

    final def map[B](f: A => B): Branch.Read[S, B] = Read.Modify(self = this, f)

    def mapK[G[_]](fK: [A] => S[A] => G[A]): Branch.Read[G, A]

  object Read:
    final case class Modify[S[_], A, B](self: Branch.Read[S, A], f: A => B) extends Branch.Read[S, B]:
      export self.{name, schema}

      override def mapK[G[_]](fK: [A] => S[A] => G[A]): Branch.Read[G, B] = copy(self = self.mapK(fK))

    given [S[_]] => Functor[Branch.Read[S, *]]:
      override def map[A, B](fa: Branch.Read[S, A])(f: A => B): Branch.Read[S, B] = fa.map(f)

    given [S[_]] => BranchOperation.Read[Branch.Read[S, *], S]:
      override def lift[A](name: String, schema: Reference[S, A]): Branch.Read[S, A] = Root(name, schema)

      extension [A](fa: Branch.Read[S, A]) override def schema: Reference[S, ?] = fa.schema

  sealed trait Write[+S[_], -A]:
    final def contramap[B](f: B => A): Branch.Write[S, B] = Write.Modify(self = this, f)

    def mapK[G[_]](fK: [A] => S[A] => G[A]): Branch.Write[G, A]

    def name: String

    def schema: Reference[S, ?]

  object Write:
    final case class Modify[S[_], A, B](self: Branch.Write[S, A], f: B => A) extends Branch.Write[S, B]:
      export self.{name, schema}

      override def mapK[G[_]](fK: [A] => S[A] => G[A]): Branch.Write[G, B] = copy(self = self.mapK(fK))

    given [S[_]] => Contravariant[Branch.Write[S, *]]:
      override def contramap[A, B](fa: Branch.Write[S, A])(f: B => A): Branch.Write[S, B] = fa.contramap(f)

    given [S[_]] => BranchOperation.Write[Branch.Write[S, *], S]:
      override def lift[A](name: String, schema: Reference[S, A]): Branch.Write[S, A] = Root(name, schema)

      extension [A](fa: Branch.Write[S, A]) override def schema: Reference[S, ?] = fa.schema

  final case class Modify[S[_], A, B](self: Branch[S, A], f: A => B, g: B => A) extends Branch[S, B]:
    export self.{name, schema}

    override def mapK[G[_]](fK: [A] => S[A] => G[A]): Branch[G, B] = copy(self = self.mapK(fK))

  final case class Root[S[_], A](name: String, schema: Reference[S, A]) extends Branch[S, A]:
    override def mapK[G[_]](fK: [A] => S[A] => G[A]): Branch[G, A] = copy(schema = schema.mapK[S, G](fK))

  given [S[_]] => Invariant[Branch[S, *]]:
    override def imap[A, B](fa: Branch[S, A])(f: A => B)(g: B => A): Branch[S, B] = fa.imap(f)(g)

  given [S[_]] => BranchOperation[Branch[S, *], S]:
    override def lift[A](name: String, schema: Reference[S, A]): Branch[S, A] = Root(name, schema)

    extension [A](fa: Branch[S, A]) override def schema: Reference[S, ?] = fa.schema
