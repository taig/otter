package io.taig.otter.http

import io.taig.otter.*
import cats.data.Chain

sealed abstract class Queries[A] extends Product with Serializable:
  def toChain: Chain[Query[?]]

  final def imap[B](f: A => B)(g: B => A): Queries[B] = Queries.Modify(self = this, f, g)

  final def zip[B](queries: Queries[B]): Queries[(A, B)] = Queries.Zip(left = this, right = queries)

  final def &[B](query: Query[B])(using merge: Merge[A, B]): Queries[merge.Out] =
    zip(queries = query.toQueries).imap(merge.apply)(merge.unapply)

object Queries:
  private[otter] case object Empty extends Queries[Unit]:
    override def toChain: Chain[Query[?]] = Chain.empty

  final private[otter] case class Root[A](query: Query[A]) extends Queries[A]:
    override def toChain: Chain[Query[?]] = Chain.one(query)

  final private[otter] case class Modify[A, B](self: Queries[A], f: A => B, g: B => A) extends Queries[B]:
    export self.toChain

  final private[otter] case class Optional[A](self: Queries[A]) extends Queries[Option[A]]:
    export self.toChain

  final private[otter] case class Zip[A, B](left: Queries[A], right: Queries[B]) extends Queries[(A, B)]:
    override def toChain: Chain[Query[?]] = left.toChain ++ right.toChain

  given invariant: Invariant.Product[Queries, Query, Queries] with
    override def fromElement[A](query: Query[A]): Queries[A] = query.toQueries
    override def result: Invariant[Queries] = this

    extension [A](self: Queries[A])
      override def imap[B](f: A => B)(g: B => A): Queries[B] = self.imap(f)(g)
      override def zip[B](queries: Queries[B]): Queries[(A, B)] = self.zip(queries)
