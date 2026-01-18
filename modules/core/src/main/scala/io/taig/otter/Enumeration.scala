package io.taig.otter

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.data.NonEmptyChain
import io.taig.enumeration.ext.Mapping
import io.taig.otter.operation.EnumerationOperation

sealed abstract class Enumeration[+F[_], A] extends Enumeration.Read[F, A], Enumeration.Write[F, A]:
  final def imap[B](f: A => B)(g: B => A): Enumeration[F, B] = Enumeration.Modify(self = this, f, g)

  def mapK[G[_]](fK: [A] => F[A] => G[A]): Enumeration[G, A]

object Enumeration:
  sealed trait Read[+F[_], +A]:
    def schema: Reference[F, ?]

    def values: NonEmptyChain[A]

    def mapK[G[_]](fK: [A] => F[A] => G[A]): Enumeration.Read[G, A]

    final def map[B](f: A => B): Enumeration.Read[F, B] = Read.Modify(self = this, f)

  object Read:
    final case class Modify[F[_], A, B](self: Enumeration.Read[F, A], f: A => B) extends Enumeration.Read[F, B]:
      export self.schema

      override def values: NonEmptyChain[B] = self.values.map(f)

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Enumeration.Read[G, B] = copy(self = self.mapK(fK))

    given [F[_]] => Functor[Enumeration.Read[F, *]]:
      override def map[A, B](fa: Enumeration.Read[F, A])(f: A => B): Enumeration.Read[F, B] = fa.map(f)

    given [F[_]] => EnumerationOperation.Read[Enumeration.Read[F, *], F]:
      override def lift[A, B](schema: Reference[F, A], mapping: Mapping[B, A]): Enumeration.Read[F, B] =
        Root(schema, mapping)

      extension [A](fa: Enumeration.Read[F, A]) override def schema: Reference[F, ?] = fa.schema

  sealed trait Write[+F[_], -A]:
    def schema: Reference[F, ?]

    def mapK[G[_]](fK: [A] => F[A] => G[A]): Enumeration.Write[G, A]

    final def contramap[B](f: B => A): Enumeration.Write[F, B] = Write.Modify(self = this, f)

  object Write:
    final case class Modify[F[_], A, B](self: Enumeration.Write[F, A], f: B => A) extends Enumeration.Write[F, B]:
      export self.schema

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Enumeration.Write[G, B] = copy(self = self.mapK(fK))

    given [F[_]] => Contravariant[Enumeration.Write[F, *]]:
      override def contramap[A, B](fa: Enumeration.Write[F, A])(f: B => A): Enumeration.Write[F, B] = fa.contramap(f)

    given [F[_]] => EnumerationOperation.Write[Enumeration.Write[F, *], F]:
      override def lift[A, B](schema: Reference[F, A], mapping: Mapping[B, A]): Enumeration.Write[F, B] =
        Root(schema, mapping)

      extension [A](fa: Enumeration.Write[F, A]) override def schema: Reference[F, ?] = fa.schema

  final case class Modify[F[_], A, B](self: Enumeration[F, A], f: A => B, g: B => A) extends Enumeration[F, B]:
    export self.schema

    override def values: NonEmptyChain[B] = self.values.map(f)

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Enumeration[G, B] = copy(self = self.mapK(fK))

  final case class Root[F[_], A, B](schema: Reference[F, A], mapping: Mapping[B, A]) extends Enumeration[F, B]:
    override def values: NonEmptyChain[B] = NonEmptyChain.fromNonEmptyList(mapping.values)

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Enumeration[G, B] = copy(schema = schema.mapK[F, G](fK))

  given [F[_]] => Invariant[Enumeration[F, *]]:
    override def imap[A, B](self: Enumeration[F, A])(f: A => B)(g: B => A): Enumeration[F, B] = self.imap(f)(g)

  given [F[_]] => EnumerationOperation[Enumeration[F, *], F]:
    override def lift[A, B](schema: Reference[F, A], mapping: Mapping[B, A]): Enumeration[F, B] =
      Root(schema, mapping)

    extension [A](fa: Enumeration[F, A]) override def schema: Reference[F, ?] = fa.schema
