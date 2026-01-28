package io.taig.otter

import cats.Contravariant
import cats.Functor
import cats.Invariant
import io.taig.otter.operation.BranchOperation

sealed abstract class Branch[+F[_], A] extends Branch.Read[F, A], Branch.Write[F, A]:
  override def mapK[G[_]](fK: [A] => F[A] => G[A]): Branch[G, A]

object Branch:
  sealed trait Read[+F[_], +A]:
    def name: String

    def schema: Reference[F, ?]

    final def map[B](f: A => B): Branch.Read[F, B] = Read.Modify(self = this, f)

    def mapK[G[_]](fK: [A] => F[A] => G[A]): Branch.Read[G, A]

  object Read:
    final case class Modify[F[_], A, B](self: Branch.Read[F, A], f: A => B) extends Branch.Read[F, B]:
      export self.{name, schema}

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Branch.Read[G, B] = copy(self = self.mapK(fK))

    given [F[_]] => Functor[Branch.Read[F, *]]:
      override def map[A, B](fa: Branch.Read[F, A])(f: A => B): Branch.Read[F, B] = fa.map(f)

    given [F[_]] => BranchOperation.Read[Branch.Read[F, *], F]:
      override def lift[A](name: String, schema: Reference[F, A]): Branch.Read[F, A] = Root(name, schema)

      extension [A](fa: Branch.Read[F, A])
        override def name: String = fa.name

        override def schema: Reference[F, ?] = fa.schema

  sealed trait Write[+F[_], -A]:
    final def contramap[B](f: B => A): Branch.Write[F, B] = Write.Modify(self = this, f)

    def mapK[G[_]](fK: [A] => F[A] => G[A]): Branch.Write[G, A]

    def name: String

    def schema: Reference[F, ?]

  object Write:
    final case class Modify[F[_], A, B](self: Branch.Write[F, A], f: B => A) extends Branch.Write[F, B]:
      export self.{name, schema}

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Branch.Write[G, B] = copy(self = self.mapK(fK))

    given [F[_]] => Contravariant[Branch.Write[F, *]]:
      override def contramap[A, B](fa: Branch.Write[F, A])(f: B => A): Branch.Write[F, B] = fa.contramap(f)

    given [F[_]] => BranchOperation.Write[Branch.Write[F, *], F]:
      override def lift[A](name: String, schema: Reference[F, A]): Branch.Write[F, A] = Root(name, schema)

      extension [A](fa: Branch.Write[F, A])
        override def name: String = fa.name

        override def schema: Reference[F, ?] = fa.schema

  final case class Modify[F[_], A, B](self: Branch[F, A], f: A => B, g: B => A) extends Branch[F, B]:
    export self.{name, schema}

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Branch[G, B] = copy(self = self.mapK(fK))

  final case class Root[F[_], A](name: String, schema: Reference[F, A]) extends Branch[F, A]:
    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Branch[G, A] = copy(schema = schema.mapK[F, G](fK))

  given [F[_]] => Invariant[Branch[F, *]]:
    override def imap[A, B](self: Branch[F, A])(f: A => B)(g: B => A): Branch[F, B] = Modify(self, f, g)

  given [F[_]] => BranchOperation[Branch[F, *], F]:
    override def lift[A](name: String, schema: Reference[F, A]): Branch[F, A] = Root(name, schema)

    extension [A](fa: Branch[F, A])
      override def name: String = fa.name

      override def schema: Reference[F, ?] = fa.schema
