package io.taig.otter

import cats.Functor
import cats.Contravariant
import cats.Invariant
import io.taig.otter.operation.CoerceOperation

type Coerce[+F[_], A] = Coerce.Read[F, A] & Coerce.Write[F, A]

object Coerce:
  sealed trait Read[+F[_], +A]:
    final def map[B](f: A => B): Coerce.Read[F, B] = Read.Modify(self = this, f)

    def schema: Reference[F, ?]

  object Read:
    final case class Modify[F[_], A, B](self: Coerce.Read[F, A], f: A => B) extends Coerce.Read[F, B]:
      export self.schema

    given [F[_]] => Functor[Coerce.Read[F, *]]:
      override def map[A, B](self: Coerce.Read[F, A])(f: A => B): Coerce.Read[F, B] = self.map(f)

    given [F[_]] => CoerceOperation.Read[Coerce.Read[F, *], F]:
      override def lift[A](schema: Reference[F, A]): Coerce.Read[F, A] = Root(schema)

      extension [A](self: Coerce.Read[F, A]) override def schema: Reference[F, ?] = self.schema

  sealed trait Write[+F[_], -A]:
    final def contramap[B](f: B => A): Coerce.Write[F, B] = Write.Modify(self = this, f)

    def schema: Reference[F, ?]

  object Write:
    final case class Modify[F[_], A, B](self: Coerce.Write[F, A], f: B => A) extends Coerce.Write[F, B]:
      export self.schema

    given [F[_]] => Contravariant[Coerce.Write[F, *]]:
      override def contramap[A, B](self: Coerce.Write[F, A])(f: B => A): Coerce.Write[F, B] = self.contramap(f)

    given [F[_]] => CoerceOperation.Write[Coerce.Write[F, *], F]:
      override def lift[A](schema: Reference[F, A]): Coerce.Write[F, A] = Root(schema)

      extension [A](self: Coerce.Write[F, A]) override def schema: Reference[F, ?] = self.schema

  final case class Modify[F[_], A, B](self: Coerce[F, A], f: A => B, g: B => A)
      extends Coerce.Read[F, B],
        Coerce.Write[F, B]:
    export self.schema

  final case class Root[F[_], A](schema: Reference[F, A]) extends Coerce.Read[F, A], Coerce.Write[F, A]

  given [F[_]] => Invariant[Coerce[F, *]]:
    override def imap[A, B](self: Coerce[F, A])(f: A => B)(g: B => A): Coerce[F, B] = Modify(self, f, g)

  given [F[_]] => CoerceOperation[Coerce[F, *], F]:
    override def lift[A](schema: Reference[F, A]): Coerce[F, A] = Root(schema)

    extension [A](self: Coerce[F, A]) override def schema: Reference[F, ?] = self.schema
