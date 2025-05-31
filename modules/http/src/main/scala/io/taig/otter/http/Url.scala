package io.taig.otter.http

import cats.data.Chain
import io.taig.otter.Merge
import io.taig.otter.Metadata
import io.taig.otter.operation.Enriched
import io.taig.otter.operation.SchemaInvariant

final case class Url[A](value: Url.Value[A], metadata: Metadata):
  def path: Path[?] = value.path

  def queries: Queries[?] = value.queries

  def zip[B](url: Url[B]): Url[(A, B)] = Url(value.zip(url.value), metadata = Metadata.Empty)

  def *[B](url: Url[B])(using merge: Merge[A, B]): Url[merge.Out] = zip(url).merge

  def /[B](parameter: Parameter[B])(using merge: Merge[A, B]): Url[merge.Out] = this * parameter.toPath.toUrl

  def /[B](name: String): Url[A] = this * Path(value = Path.Value.Static(name), metadata = Metadata.Empty).toUrl

  def &[B](query: Query[B])(using merge: Merge[A, B]): Url[merge.Out] = this * query.toQueries.toUrl

object Url:
  sealed abstract class Value[A] extends Product with Serializable:
    def path: Path[?]

    def queries: Queries[?]

    final def imap[B](f: A => B)(g: B => A): Url.Value[B] = Url.Value.Modify(self = this, f, g)

    final def zip[B](url: Url.Value[B]): Url.Value[(A, B)] = Url.Value.Zip(left = this, right = url)

  object Value:
    private[otter] case object Empty extends Url.Value[Unit]:
      override def path: Path[?] = Path.Empty
      override def queries: Queries[?] = Queries.Empty

    final private[otter] case class Modify[A, B](self: Url.Value[A], f: A => B, g: B => A) extends Url.Value[B]:
      export self.{path, queries}

    final private[otter] case class Root[A, B](path: Path[A], queries: Queries[B]) extends Url.Value[(A, B)]

    final private[otter] case class Zip[A, B](left: Url.Value[A], right: Url.Value[B]) extends Url.Value[(A, B)]:
      override def path: Path[?] = left.path.zip(right.path)
      override def queries: Queries[?] = left.queries.zip(right.queries)

  final case class Data(path: Path.Data, queries: Queries.Data):
    def combine(url: Url.Data): Url.Data =
      Url.Data(path = path ++ url.path, queries = queries ++ url.queries)

  object Data:
    val Empty: Url.Data = Data(path = Chain.empty, queries = Chain.empty)

  val Empty: Url[Unit] = Url(value = Url.Value.Empty, metadata = Metadata.Empty)

  given SchemaInvariant[Url] with
    override def imap[A, B](fa: Url[A])(f: A => B)(g: B => A): Url[B] =
      fa.copy(value = fa.value.imap(f)(g))

    override def enriched[A]: Enriched[Url[A]] = new Enriched[Url[A]]:
      override def metadata(a: Url[A]): Metadata = a.metadata
      override def modifyMetadata(a: Url[A])(f: Metadata => Metadata): Url[A] =
        a.copy(metadata = f(a.metadata))
