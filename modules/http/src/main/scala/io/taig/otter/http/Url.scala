package io.taig.otter.http

sealed abstract class Url[A] extends Product with Serializable:
  def path: Path[?]

  def queries: Queries[?]

  final def imap[B](f: A => B)(g: B => A): Url[B] = Url.Modify(self = this, f, g)

  final def zip[B](url: Url[B]): Url[(A, B)] = Url.Zip(left = this, right = url)

object Url:
  private[otter] case object Empty extends Url[Unit]:
    override def path: Path[?] = Path.Empty
    override def queries: Queries[?] = Queries.Empty

  final private[otter] case class Modify[A, B](self: Url[A], f: A => B, g: B => A) extends Url[B]:
    export self.{path, queries}

  final private[otter] case class Root[A, B](path: Path[A], queries: Queries[B]) extends Url[(A, B)]

  final private[otter] case class Zip[A, B](left: Url[A], right: Url[B]) extends Url[(A, B)]:
    override def path: Path[?] = left.path.zip(right.path)
    override def queries: Queries[?] = left.queries.zip(right.queries)
