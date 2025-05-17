package io.taig.otter.http

import cats.Show
import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.Merge
import io.taig.otter.Metadata
import cats.Invariant

sealed abstract class Path[A] extends Product with Serializable:
  def toSegments: Chain[Segment[?]]

  final def imap[B](f: A => B)(g: B => A): Path[B] = Path.Modify(self = this, f, g)

  final def zip[B](path: Path[B]): Path[(A, B)] = Path.Zip(left = this, right = path)

  final def /[B](segment: Segment[B])(using merge: Merge[A, B]): Path[merge.Out] =
    zip(segment.toPath).imap(merge.apply)(merge.unapply)
  final def /[B](name: String): Path[A] = /(Segment.Static(name, metadata = Metadata.Empty))

  final def toUrl: Url[A] = Url.Root(path = this, queries = Queries.Empty).imap((a, _) => a)((_, ()))

object Path:
  private[otter] case object Empty extends Path[Unit]:
    override def toSegments: Chain[Segment[?]] = Chain.empty

  final private[otter] case class Modify[A, B](self: Path[A], f: A => B, g: B => A) extends Path[B]:
    export self.toSegments

  final private[otter] case class Root[A](segment: Segment[A]) extends Path[A]:
    override def toSegments: Chain[Segment[?]] = Chain.one(segment)

  final private[otter] case class Zip[A, B](left: Path[A], right: Path[B]) extends Path[(A, B)]:
    override def toSegments: Chain[Segment[?]] = left.toSegments ++ right.toSegments

  type Data = Chain[String]

  given Invariant[Path] with
    override def imap[A, B](fa: Path[A])(f: A => B)(g: B => A): Path[B] = fa.imap(f)(g)

  given Show[Path[?]] = _.toSegments.mkString_("/", "/", "")
