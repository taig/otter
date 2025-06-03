package io.taig.otter.http
import cats.data.Chain
import io.taig.otter.*
import io.taig.otter.operation.Enriched
import io.taig.otter.operation.SchemaInvariant

final case class Queries[A](value: Queries.Value[A], metadata: Metadata):
  def toChain: Chain[Query[?]] = value.toChain

  def zip[B](queries: Queries[B]): Queries[(A, B)] =
    Queries(value = value.zip(queries.value), metadata = Metadata.Empty)

  def merge[B](queries: Queries[B])(using m: Merge[A, B]): Queries[m.Out] = zip(queries).merged

  def &[B](query: Query[B])(using m: Merge[A, B]): Queries[m.Out] = merge(query.toQueries)

  def optional: Queries[Option[A]] = Queries(value = value.optional, metadata = Metadata.Empty)

  def optional(default: A): Queries[A] = Queries(value = value.optional(default), metadata = Metadata.Empty)

  def toUrl: Url[A] = Url(
    value = Url.Value.Root(path = Path.Empty, queries = this).imap((_, a) => a)(a => ((), a)),
    metadata = Metadata.Empty
  )

object Queries:
  sealed abstract class Value[A] extends Product, Serializable:
    def toChain: Chain[Query[?]]

    final def imap[B](f: A => B)(g: B => A): Queries.Value[B] = Value.Modify(self = this, f, g)

    final def optional: Queries.Value[Option[A]] = Value.Optional(self = this)
    
    final def optional(default: A): Queries.Value[A] = Value.Default(self = this, value = default)

    final def zip[B](queries: Queries.Value[B]): Queries.Value[(A, B)] =
      Value.Zip(left = this, right = queries)

  object Value:
    private[otter] final case class Default[A](self: Queries.Value[A], value: A) extends Queries.Value[A]:
      export self.toChain

    private[otter] case object Empty extends Queries.Value[Unit]:
      override def toChain: Chain[Query[?]] = Chain.empty

    final private[otter] case class Modify[A, B](self: Queries.Value[A], f: A => B, g: B => A) extends Queries.Value[B]:
      export self.toChain

    final private[otter] case class Optional[A](self: Queries.Value[A]) extends Queries.Value[Option[A]]:
      export self.toChain

    final private[otter] case class Root[A](query: Query[A]) extends Queries.Value[A]:
      override def toChain: Chain[Query[?]] = Chain.one(query)

    final private[otter] case class Zip[A, B](left: Queries.Value[A], right: Queries.Value[B])
        extends Queries.Value[(A, B)]:
      override def toChain: Chain[Query[?]] = left.toChain ++ right.toChain

  type Data = Chain[Query.Data]

  val Empty: Queries[Unit] = Queries(value = Value.Empty, metadata = Metadata.Empty)

  given SchemaInvariant[Queries] with
    override def imap[A, B](fa: Queries[A])(f: A => B)(g: B => A): Queries[B] =
      fa.copy(value = fa.value.imap(f)(g))

    override def enriched[A]: Enriched[Queries[A]] = new Enriched[Queries[A]]:
      override def metadata(a: Queries[A]): Metadata = a.metadata
      override def modifyMetadata(a: Queries[A])(f: Metadata => Metadata): Queries[A] =
        a.copy(metadata = f(a.metadata))
