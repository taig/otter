package io.taig.otter.http

import cats.data.Chain
import io.taig.otter.Reference
import cats.Apply
import cats.ContravariantSemigroupal
import cats.InvariantSemigroupal
import io.taig.otter.http.operation.QueriesOperation

sealed abstract class Queries[+F[_], A] extends Queries.Read[F, A], Queries.Write[F, A]:
  final def product[F1[a] >: F[a], B](queries: Queries[F1, B]): Queries[F1, (A, B)] =
    Queries.Product(left = this, right = queries)

  override def queries: Chain[Reference[F, ?]]

object Queries:
  sealed trait Read[+F[_], +A]:
    final def product[F1[a] >: F[a], B](queries: Queries.Read[F1, B]): Queries.Read[F1, (A, B)] =
      Queries.Read.Product(left = this, right = queries)

    def queries: Chain[Reference[F, ?]]

  object Read:
    final case class Modify[F[_], A, B](self: Queries.Read[F, A], f: A => B) extends Queries.Read[F, B]:
      export self.queries

    final case class Product[F[_], A, B](left: Queries.Read[F, A], right: Queries.Read[F, B])
        extends Queries.Read[F, (A, B)]:
      override def queries: Chain[Reference[F, ?]] = left.queries ++ right.queries

    given [F[_]] => Apply[Queries.Read[F, *]]:
      override def ap[A, B](ff: Queries.Read[F, A => B])(fa: Queries.Read[F, A]): Queries.Read[F, B] =
        map(Product(ff, fa))(_ apply _)

      override def map[A, B](self: Queries.Read[F, A])(f: A => B): Queries.Read[F, B] = Modify(self, f)

    given [F[_]] => QueriesOperation.Read[Queries.Read[F, *], F]:
      override def empty: Queries.Read[Nothing, Unit] = Empty

      override def lift[A](reference: Reference[F, A]): Queries.Read[F, A] = Root(reference)

  sealed trait Write[+F[_], -A]:
    final def product[F1[a] >: F[a], B](queries: Queries.Write[F1, B]): Queries.Write[F1, (A, B)] =
      Queries.Write.Product(left = this, right = queries)

    def queries: Chain[Reference[F, ?]]

  object Write:
    final case class Modify[F[_], A, B](self: Queries.Write[F, A], f: B => A) extends Queries.Write[F, B]:
      export self.queries

    final case class Product[F[_], A, B](left: Queries.Write[F, A], right: Queries.Write[F, B])
        extends Queries.Write[F, (A, B)]:
      override def queries: Chain[Reference[F, ?]] =
        left.queries ++ right.queries

    given [F[_]] => ContravariantSemigroupal[Queries.Write[F, *]]:
      override def product[A, B](fa: Queries.Write[F, A], fb: Queries.Write[F, B]): Queries.Write[F, (A, B)] =
        Product(fa, fb)

      override def contramap[A, B](self: Queries.Write[F, A])(f: B => A): Queries.Write[F, B] = Modify(self, f)

    given [F[_]] => QueriesOperation.Write[Queries.Write[F, *], F]:
      override def empty: Queries.Write[Nothing, Unit] = Empty

      override def lift[A](reference: Reference[F, A]): Queries.Write[F, A] = Root(reference)

  case object Empty extends Queries[Nothing, Unit]:
    override def queries: Chain[Nothing] = Chain.empty

  final case class Modify[F[_], A, B](self: Queries[F, A], f: A => B, g: B => A) extends Queries[F, B]:
    override def queries: Chain[Reference[F, ?]] = self.queries

  final case class Product[F[_], A, B](left: Queries[F, A], right: Queries[F, B]) extends Queries[F, (A, B)]:
    override def queries: Chain[Reference[F, ?]] = left.queries ++ right.queries

  final case class Root[F[_], A](query: Reference[F, A]) extends Queries[F, A]:
    override def queries: Chain[Reference[F, ?]] = Chain.one(query)

  given [F[_]] => InvariantSemigroupal[Queries[F, *]]:
    override def imap[A, B](self: Queries[F, A])(f: A => B)(g: B => A): Queries[F, B] = Modify(self, f, g)

    override def product[A, B](fa: Queries[F, A], fb: Queries[F, B]): Queries[F, (A, B)] = Product(fa, fb)

  given [F[_]] => QueriesOperation[Queries[F, *], F]:
    override def empty: Queries[Nothing, Unit] = Empty

    override def lift[A](reference: Reference[F, A]): Queries[F, A] = Root(reference)
