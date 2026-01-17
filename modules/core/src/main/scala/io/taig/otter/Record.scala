package io.taig.otter

import cats.Apply
import cats.ContravariantSemigroupal
import cats.InvariantSemigroupal
import cats.data.Chain
import io.taig.otter.operation.RecordOperation

sealed abstract class Record[+S[_], A] extends Record.Read[S, A], Record.Write[S, A]:
  final def imap[B](f: A => B)(g: B => A): Record[S, B] = Record.Modify(self = this, f, g)

  def mapK[G[_]](fK: [A] => S[A] => G[A]): Record[G, A]

  final def product[S1[a] >: S[a], B](schema: Record[S1, B]): Record[S1, (A, B)] =
    Record.Product(left = this, right = schema)

object Record:
  sealed trait Read[+S[_], +A]:
    def fields: Chain[Reference[S, ?]]

    def mapK[G[_]](fK: [A] => S[A] => G[A]): Record.Read[G, A]

    final def map[B](f: A => B): Record.Read[S, B] = Read.Modify(self = this, f)

    final def product[S1[a] >: S[a], B](schema: Record.Read[S1, B]): Record.Read[S1, (A, B)] =
      Read.Product(left = this, right = schema)

  object Read:
    final case class Modify[S[_], A, B](self: Record.Read[S, A], f: A => B) extends Record.Read[S, B]:
      export self.fields

      override def mapK[G[_]](fK: [A] => S[A] => G[A]): Record.Read[G, B] = copy(self = self.mapK(fK))

    final case class Product[S[_], A, B](left: Record.Read[S, A], right: Record.Read[S, B])
        extends Record.Read[S, (A, B)]:
      override def fields: Chain[Reference[S, ?]] = left.fields ++ right.fields

      override def mapK[G[_]](fK: [A] => S[A] => G[A]): Record.Read[G, (A, B)] =
        copy(left = left.mapK(fK), right = right.mapK(fK))

    given [F[_]] => Apply[Record.Read[F, *]]:
      override def ap[A, B](ff: Record.Read[F, A => B])(fa: Record.Read[F, A]): Record.Read[F, B] =
        ff.product(fa).map(_ apply _)

      override def map[A, B](fa: Record.Read[F, A])(f: A => B): Record.Read[F, B] = fa.map(f)

    given [F[_]] => RecordOperation.Read[Record.Read[F, *], F]:
      override def empty: Record.Read[F, Unit] = Empty

      override def lift[A](field: Reference[F, A]): Record.Read[F, A] = Root(field)

      extension [A](fa: Record.Read[F, A]) override def fields: Chain[Reference[F, ?]] = fa.fields

  sealed trait Write[+S[_], -A]:
    def fields: Chain[Reference[S, ?]]

    def mapK[G[_]](fK: [A] => S[A] => G[A]): Record.Write[G, A]

    final def contramap[B](f: B => A): Record.Write[S, B] = Write.Modify(self = this, f)

    final def product[S1[a] >: S[a], B](schema: Record.Write[S1, B]): Record.Write[S1, (A, B)] =
      Write.Product(left = this, right = schema)

  object Write:
    final case class Modify[S[_], A, B](self: Record.Write[S, A], f: B => A) extends Record.Write[S, B]:
      export self.fields

      override def mapK[G[_]](fK: [A] => S[A] => G[A]): Record.Write[G, B] = copy(self = self.mapK(fK))

    final case class Product[S[_], A, B](left: Record.Write[S, A], right: Record.Write[S, B])
        extends Record.Write[S, (A, B)]:
      override def fields: Chain[Reference[S, ?]] = left.fields ++ right.fields

      override def mapK[G[_]](fK: [A] => S[A] => G[A]): Record.Write[G, (A, B)] =
        copy(left = left.mapK(fK), right = right.mapK(fK))

    given [F[_]] => ContravariantSemigroupal[Record.Write[F, *]]:
      override def product[A, B](fa: Record.Write[F, A], fb: Record.Write[F, B]): Record.Write[F, (A, B)] =
        fa.product(fb)

      override def contramap[A, B](fa: Record.Write[F, A])(f: B => A): Record.Write[F, B] = fa.contramap(f)

    given [F[_]] => RecordOperation.Write[Record.Write[F, *], F]:
      override def empty: Record.Write[F, Unit] = Empty

      override def lift[A](field: Reference[F, A]): Record.Write[F, A] = Root(field)

      extension [A](fa: Record.Write[F, A]) override def fields: Chain[Reference[F, ?]] = fa.fields

  case object Empty extends Record[Nothing, Unit]:
    override def fields: Chain[Reference[Nothing, ?]] = Chain.empty

    override def mapK[G[_]](fK: [A] => Nothing => G[A]): Record[G, Unit] = this

  final case class Modify[S[_], A, B](self: Record.Write[S, A], f: A => B, g: B => A) extends Record[S, B]:
    export self.fields

    override def mapK[G[_]](fK: [A] => S[A] => G[A]): Record[G, B] = copy(self = self.mapK(fK))

  final case class Product[S[_], A, B](left: Record[S, A], right: Record[S, B]) extends Record[S, (A, B)]:
    override def fields: Chain[Reference[S, ?]] = left.fields ++ right.fields

    override def mapK[G[_]](fK: [A] => S[A] => G[A]): Record[G, (A, B)] =
      copy(left = left.mapK(fK), right = right.mapK(fK))

  final case class Root[S[_], A](field: Reference[S, A]) extends Record[S, A]:
    override def fields: Chain[Reference[S, ?]] = Chain.one(field)

    override def mapK[G[_]](fK: [A] => S[A] => G[A]): Record[G, A] = copy(field = field.mapK[S, G](fK))

  given [F[_]] => InvariantSemigroupal[Record[F, *]]:
    override def imap[A, B](fa: Record[F, A])(f: A => B)(g: B => A): Record[F, B] = fa.imap(f)(g)

    override def product[A, B](fa: Record[F, A], fb: Record[F, B]): Record[F, (A, B)] = fa.product(fb)

  given [F[_]] => RecordOperation[Record[F, *], F]:
    override def empty: Record[F, Unit] = Empty

    override def lift[A](field: Reference[F, A]): Record[F, A] = Root(field)

    extension [A](fa: Record[F, A]) override def fields: Chain[Reference[F, ?]] = fa.fields
