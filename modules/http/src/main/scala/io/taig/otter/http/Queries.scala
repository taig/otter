package io.taig.otter.http

import cats.Invariant
import cats.data.Chain
import io.taig.otter.*

type Queries[A] = Enrichment[Queries.Value, A]

object Queries:
  sealed abstract class Value[A] extends Product with Serializable:
    def toChain: Chain[Query[?]]

    final def imap[B](f: A => B)(g: B => A): Queries.Value[B] = Value.Modify(self = this, f, g)

    final def zip[B](queries: Queries.Value[B]): Queries.Value[(A, B)] =
      Value.Zip(left = this, right = queries)

    final def &[B](query: Query[B])(using merge: Merge[A, B]): Queries.Value[merge.Out] = ???
    // zip(queries = query.toQueries).imap(merge.apply)(merge.unapply)

  object Value:
    private[otter] case object Empty extends Queries.Value[Unit]:
      override def toChain: Chain[Query[?]] = Chain.empty

    final private[otter] case class Root[A](query: Query[A]) extends Queries.Value[A]:
      override def toChain: Chain[Query[?]] = Chain.one(query)

    final private[otter] case class Modify[A, B](self: Queries.Value[A], f: A => B, g: B => A) extends Queries.Value[B]:
      export self.toChain

    final private[otter] case class Optional[A](self: Queries.Value[A]) extends Queries.Value[Option[A]]:
      export self.toChain

    final private[otter] case class Zip[A, B](left: Queries.Value[A], right: Queries.Value[B])
        extends Queries.Value[(A, B)]:
      override def toChain: Chain[Query[?]] = left.toChain ++ right.toChain

  val Empty: Queries[Unit] = Enrichment(Value.Empty)

  type Data = Chain[Query.Data]

  // given Invariant[Queries] with
  //   override def imap[A, B](fa: Value[A])(f: A => B)(g: B => A): Value[B] = fa.imap(f)(g)
