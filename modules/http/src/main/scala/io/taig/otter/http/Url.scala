package io.taig.otter.http

import cats.data.Chain
import io.taig.otter.Merge
import io.taig.otter.Enrichment

type Url[A] = Enrichment[Url.Value, A]

object Url:
  sealed abstract class Value[A] extends Product with Serializable:
    def path: Path[?]

    def queries: Queries[?]

    final def imap[B](f: A => B)(g: B => A): Url.Value[B] = Url.Value.Modify(self = this, f, g)

    final def zip[B](url: Url.Value[B]): Url.Value[(A, B)] = Url.Value.Zip(left = this, right = url)

    final def /[B](parameter: Parameter[B])(using merge: Merge[A, B]): Url.Value[merge.Out] = ???
    // zip(parameter.toPath.toUrl).imap(merge.apply)(merge.unapply)

    // final def /[B](name: String): Url[A] = zip(Path.Static(name).toUrl).imap(((a, _) => a))(a => (a, ()))

  object Value:
    private[otter] case object Empty extends Url.Value[Unit]:
      override def path: Path[?] = Path.Empty
      override def queries: Queries[?] = Queries.Empty

    final private[otter] case class Modify[A, B](self: Url.Value[A], f: A => B, g: B => A) extends Url.Value[B]:
      export self.{path, queries}

    final private[otter] case class Root[A, B](path: Path[A], queries: Queries[B]) extends Url.Value[(A, B)]

    final private[otter] case class Zip[A, B](left: Url.Value[A], right: Url.Value[B]) extends Url.Value[(A, B)]:
      override def path: Path[?] = ??? // left.path.zip(right.path)
      override def queries: Queries[?] = ??? // left.queries.zip(right.queries)

  val Empty: Url[Unit] = Enrichment(Url.Value.Empty)

  final case class Data(path: Path.Data, queries: Queries.Data):
    def combine(url: Url.Data): Url.Data =
      Url.Data(path = path ++ url.path, queries = queries ++ url.queries)

  object Data:
    val Empty: Url.Data = Data(path = Chain.empty, queries = Chain.empty)
