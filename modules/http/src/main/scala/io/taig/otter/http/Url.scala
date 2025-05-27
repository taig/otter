package io.taig.otter.http

import cats.data.Chain
import io.taig.otter.operation.EnrichedSchemaInvariant
import io.taig.otter.Merge
import io.taig.otter.Metadata
import io.taig.otter.Enrichment

type Url[A] = Enrichment[Url.Value, A]

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

  val Empty: Url[Unit] = Enrichment(Url.Value.Empty)

  extension [A](self: Url[A])
    def path: Path[?] = self.self.path
    def queries: Queries[?] = self.self.queries

    def zip[B](url: Url[B]): Url[(A, B)] = Enrichment(self.self.zip(url.self))

    def merge[B](url: Url[B])(using merge: Merge[A, B]): Url[merge.Out] = self.zip(url).merge

    def /[B](parameter: Parameter[B])(using merge: Merge[A, B]): Url[merge.Out] = self.merge(parameter.toPath.toUrl)

    def /(name: String): Url[A] = Enrichment(
      self.self
        .zip(Url.Value.Root(path = Enrichment(Path.Value.Static(name)), queries = Queries.Empty))
        .imap((a, _) => a)((_, ((), ())))
    )

  given EnrichedSchemaInvariant[Url] with
    override def imap[A, B](fa: Url[A])(f: A => B)(g: B => A): Url[B] = fa.mapF(_.imap(f)(g))

    extension [A](self: Url[A])
      def metadata: Metadata = self.metadata
      def metadata(f: Metadata => Metadata): Url[A] = self.modifyMetadata(f)
