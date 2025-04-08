package io.taig.otter.http

import io.taig.otter.Invariant
import io.taig.otter.Merge

sealed abstract class Path[A] extends Product with Serializable:
  final def imap[B](f: A => B)(g: B => A): Path[B] = Path.Modify(self = this, f, g)
  
  final def zip[B](path: Path[B]): Path[(A, B)] = Path.Zip(left = this, right = path)
  
  final def /[B](segment: Segment[B])(using merge: Merge[A, B]): Path[merge.Out] =
    zip(segment.toPath).imap(merge.apply)(merge.unapply)
  final def /:[B](segment: Segment[B])(using merge: Merge[B, A]): Path[merge.Out] =
    segment.toPath.zip(this).imap(merge.apply)(merge.unapply)

object Path:
  private[otter] case object Empty extends Path[Unit]

  final private[otter] case class Modify[A, B](self: Path[A], f: A => B, g: B => A) extends Path[B]

  final private[otter] case class Root[A](segment: Segment[A]) extends Path[A]

  final private[otter] case class Zip[A, B](left: Path[A], right: Path[B]) extends Path[(A, B)]

  given Invariant.Product[Path, Segment, Path] with
    override def result: Invariant[Path] = this
    override def fromElement[A](segment: Segment[A]): Path[A] = Root(segment)
    
    extension [A](self: Path[A])
      override def imap[B](f: A => B)(g: B => A): Path[B] = self.imap(f)(g)
      override def zip[B](codec: Path[B]): Path[(A, B)] = ???
