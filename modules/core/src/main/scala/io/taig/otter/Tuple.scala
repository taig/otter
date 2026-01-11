package io.taig.otter

import cats.data.Chain
import cats.InvariantSemigroupal
import cats.Apply
import cats.ContravariantSemigroupal
import io.taig.otter.operation.TupleOperation

sealed abstract class Tuple[+S[_], A] extends Tuple.Read[S, A], Tuple.Write[S, A]:
  final def imap[B](f: A => B)(g: B => A): Tuple[S, B] = Tuple.Modify(self = this, f, g)

  final def product[S1[a] >: S[a], B](schema: Tuple[S1, B]): Tuple[S1, (A, B)] =
    Tuple.Product(left = this, right = schema)

object Tuple:
  sealed trait Read[+S[_], +A]:
    final def map[B](f: A => B): Tuple.Read[S, B] = Read.Modify(self = this, f)

    def schemas: Chain[Reference[S, ?]]

    final def product[S1[a] >: S[a], B](schema: Tuple.Read[S1, B]): Tuple.Read[S1, (A, B)] =
      Read.Product(left = this, right = schema)

  object Read:
    final case class Modify[S[_], A, B](self: Tuple.Read[S, A], f: A => B) extends Tuple.Read[S, B]:
      export self.schemas

    final case class Product[S[_], A, B](left: Tuple.Read[S, A], right: Tuple.Read[S, B]) extends Tuple.Read[S, (A, B)]:
      override def schemas: Chain[Reference[S, ?]] = left.schemas ++ right.schemas

    given [S[_]] => Apply[Tuple.Read[S, *]]:
      override def map[A, B](fa: Tuple.Read[S, A])(f: A => B): Tuple.Read[S, B] = fa.map(f)

      override def ap[A, B](ff: Tuple.Read[S, A => B])(fa: Tuple.Read[S, A]): Tuple.Read[S, B] =
        ff.product(fa).map(_ apply _)

    given [S[_]] => TupleOperation[Tuple.Read, S]:
      override def empty: Tuple.Read[Nothing, Unit] = Tuple.Empty

      override def lift[T[a] <: S[a], A](schema: Reference[T, A]): Tuple.Read[T, A] = ???

  sealed trait Write[+S[_], -A]:
    final def contramap[B](f: B => A): Tuple.Write[S, B] = Write.Modify(self = this, f)

    def schemas: Chain[Reference[S, ?]]

    final def product[S1[a] >: S[a], B](schema: Tuple.Write[S1, B]): Tuple.Write[S1, (A, B)] =
      Write.Product(left = this, right = schema)

  object Write:
    final case class Modify[S[_], A, B](self: Tuple.Write[S, A], f: B => A) extends Tuple.Write[S, B]:
      export self.schemas

    final case class Product[S[_], A, B](left: Tuple.Write[S, A], right: Tuple.Write[S, B])
        extends Tuple.Write[S, (A, B)]:
      override def schemas: Chain[Reference[S, ?]] = left.schemas ++ right.schemas

    given [S[_]] => ContravariantSemigroupal[Tuple.Write[S, *]]:
      override def contramap[A, B](fa: Tuple.Write[S, A])(f: B => A): Tuple.Write[S, B] =
        fa.contramap(f)

      override def product[A, B](fa: Tuple.Write[S, A], fb: Tuple.Write[S, B]): Tuple.Write[S, (A, B)] = fa.product(fb)

  case object Empty extends Tuple[Nothing, Unit]:
    override def schemas: Chain[Nothing] = Chain.empty

  final case class Modify[S[_], A, B](self: Tuple[S, A], f: A => B, g: B => A) extends Tuple[S, B]:
    export self.schemas

  final case class Product[S[_], A, B](left: Tuple[S, A], right: Tuple[S, B]) extends Tuple[S, (A, B)]:
    override def schemas: Chain[Reference[S, ?]] = left.schemas ++ right.schemas

  final case class Root[S[_], A](schema: Reference[S, A]) extends Tuple[S, A]:
    override def schemas: Chain[Reference[S, A]] = Chain.one(schema)

  given [S[_]] => InvariantSemigroupal[Tuple[S, *]]:
    override def imap[A, B](self: Tuple[S, A])(f: A => B)(g: B => A): Tuple[S, B] = self.imap(f)(g)

    override def product[A, B](fa: Tuple[S, A], fb: Tuple[S, B]): Tuple[S, (A, B)] = fa.product(fb)
