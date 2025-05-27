package io.taig.otter.http
import cats.data.Chain
import io.taig.otter.*
import io.taig.otter.operation.EnrichedSchemaInvariant

final case class Queries[A](self: Enrichment[Queries.Value[A]]) extends AnyVal:
  inline def value: Queries.Value[A] = self.self

  def toChain: Chain[Query[?]] = value.toChain

  def zip[B](queries: Queries[B]): Queries[(A, B)] = Queries(Enrichment(value.zip(queries.value)))

  def *[B](queries: Queries[B])(using merge: Merge[A, B]): Queries[merge.Out] = zip(queries).merge

  def &[B](query: Query[B])(using merge: Merge[A, B]): Queries[merge.Out] = this * query.toQueries

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

  val Empty: Queries[Unit] = Queries(Enrichment(Value.Empty))

  given EnrichedSchemaInvariant[Queries] with
    override def imap[A, B](fa: Queries[A])(f: A => B)(g: B => A): Queries[B] =
      fa.copy(self = fa.self.map(_.imap(f)(g)))

    extension [A](self: Queries[A])
      def metadata: Metadata = self.self.metadata
      def metadata(f: Metadata => Metadata): Queries[A] =
        self.copy(self = self.self.modifyMetadata(f))
