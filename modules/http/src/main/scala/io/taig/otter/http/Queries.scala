package io.taig.otter.http

import cats.data.Chain
import io.taig.otter.Reference
import cats.Apply
import cats.ContravariantSemigroupal
import cats.InvariantSemigroupal
import io.taig.otter.http.operation.QueriesOperation

sealed abstract class Queries[A] extends Queries.Read[A], Queries.Write[A]:
  final def product[B](queries: Queries[B]): Queries[(A, B)] = Queries.Product(left = this, right = queries)

  override def queries: Chain[Reference[Http.Query, ?]]

object Queries:
  sealed trait Read[+A]:
    final def product[B](queries: Queries.Read[B]): Queries.Read[(A, B)] =
      Queries.Read.Product(left = this, right = queries)

    def queries: Chain[Reference[Http.Query.Read, ?]]

  object Read:
    final case class Modify[A, B](self: Queries.Read[A], f: A => B) extends Queries.Read[B]:
      export self.queries

    final case class Product[A, B](left: Queries.Read[A], right: Queries.Read[B]) extends Queries.Read[(A, B)]:
      override def queries: Chain[Reference[Http.Query.Read, ?]] = left.queries ++ right.queries

    given Apply[Queries.Read]:
      override def ap[A, B](ff: Queries.Read[A => B])(fa: Queries.Read[A]): Queries.Read[B] =
        map(Product(ff, fa))(_ apply _)

      override def map[A, B](self: Queries.Read[A])(f: A => B): Queries.Read[B] = Modify(self, f)

  sealed trait Write[-A]:
    final def product[B](queries: Queries.Write[B]): Queries.Write[(A, B)] =
      Queries.Write.Product(left = this, right = queries)

    def queries: Chain[Reference[Http.Query.Write, ?]]

  object Write:
    final case class Modify[A, B](self: Queries.Write[A], f: B => A) extends Queries.Write[B]:
      export self.queries

    final case class Product[A, B](left: Queries.Write[A], right: Queries.Write[B]) extends Queries.Write[(A, B)]:
      override def queries: Chain[Reference[Http.Query.Write, ?]] =
        left.queries ++ right.queries

    given ContravariantSemigroupal[Queries.Write]:
      override def product[A, B](fa: Queries.Write[A], fb: Queries.Write[B]): Queries.Write[(A, B)] = Product(fa, fb)

      override def contramap[A, B](self: Queries.Write[A])(f: B => A): Queries.Write[B] = Modify(self, f)

  case object Empty extends Queries[Unit]:
    override def queries: Chain[Nothing] = Chain.empty

  final case class Modify[A, B](self: Queries[A], f: A => B, g: B => A) extends Queries[B]:
    override def queries: Chain[Reference[Http.Query, ?]] = self.queries

  final case class Product[A, B](left: Queries[A], right: Queries[B]) extends Queries[(A, B)]:
    override def queries: Chain[Reference[Http.Query, ?]] = left.queries ++ right.queries

  final case class Root[A](query: Reference[Http.Query, A]) extends Queries[A]:
    override def queries: Chain[Reference[Http.Query, ?]] = Chain.one(query)

  given InvariantSemigroupal[Queries]:
    override def imap[A, B](self: Queries[A])(f: A => B)(g: B => A): Queries[B] = Modify(self, f, g)

    override def product[A, B](fa: Queries[A], fb: Queries[B]): Queries[(A, B)] = Product(fa, fb)

  given QueriesOperation[Queries] = ???
