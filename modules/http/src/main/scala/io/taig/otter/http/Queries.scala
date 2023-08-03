package io.taig.otter.http

import cats.InvariantSemigroupal
import cats.data.Chain
import cats.syntax.all.*

sealed abstract class Queries[A]:
  def toChain: Chain[Query[?]]
  final def zip[B](other: Queries[B]): Queries[(A, B)] = Queries.Zip(this, other)
  final def imap[B](f: A => B)(g: B => A): Queries[B] = Queries.Modify(this, f, g)

object Queries:
  private[otter] case object Root extends Queries[Unit]:
    override def toChain: Chain[Query[?]] = Chain.empty

  final private[otter] case class One[A](query: Query[A]) extends Queries[A]:
    override def toChain: Chain[Query[?]] = Chain.one(query)

  final private[otter] case class Zip[A, B](left: Queries[A], right: Queries[B]) extends Queries[(A, B)]:
    override def toChain: Chain[Query[?]] = left.toChain ++ right.toChain

  final private[otter] case class Modify[A, B](self: Queries[A], f: A => B, g: B => A) extends Queries[B]:
    override def toChain: Chain[Query[?]] = self.toChain

  val Empty: Queries[Unit] = Root

  given InvariantSemigroupal[Queries] with
    override def imap[A, B](fa: Queries[A])(f: A => B)(g: B => A): Queries[B] = fa.imap(f)(g)
    override def product[A, B](fa: Queries[A], fb: Queries[B]): Queries[(A, B)] = fa.product(fb)
