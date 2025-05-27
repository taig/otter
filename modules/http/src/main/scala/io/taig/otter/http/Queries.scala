package io.taig.otter.http

import cats.Invariant
import cats.data.Chain
import io.taig.otter.*
import io.taig.otter.operation.EnrichedSchemaInvariant

type Queries[A] = Enrichment[Queries.Value, A]

object Queries:
  sealed abstract class Value[A] extends Product with Serializable:
    def toChain: Chain[Query[?]]

    final def imap[B](f: A => B)(g: B => A): Queries.Value[B] = Value.Modify(self = this, f, g)

    final def zip[B](queries: Queries.Value[B]): Queries.Value[(A, B)] =
      Value.Zip(left = this, right = queries)

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

  type Data = Chain[Query.Data]

  val Empty: Queries[Unit] = Enrichment(Value.Empty)

  extension [A](self: Queries[A])
    def toChain: Chain[Query[?]] = self.self.toChain

    def zip[B](queries: Queries[B]): Queries[(A, B)] = Enrichment(self.self.zip(queries.self))

    def merge[B](queries: Queries[B])(using merge: Merge[A, B]): Queries[merge.Out] =
      self.zip(queries).merge

    def &[B](query: Query[B])(using merge: Merge[A, B]): Queries[merge.Out] =
      self.merge(query.toQueries)

  given EnrichedSchemaInvariant[Queries] with
    override def imap[A, B](fa: Queries[A])(f: A => B)(g: B => A): Queries[B] = fa.mapF(_.imap(f)(g))

    extension [A](self: Queries[A])
      def metadata: Metadata = self.metadata
      def metadata(f: Metadata => Metadata): Queries[A] = self.modifyMetadata(f)
