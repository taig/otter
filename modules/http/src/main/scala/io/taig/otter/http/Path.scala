package io.taig.otter.http

import cats.Invariant

sealed abstract class Path[A] extends Product with Serializable:
  final def imap[B](f: A => B)(g: B => A): Path[B] = Path.Modify(self = this, f, g)

object Path:
  private[otter] case object Empty extends Path[Unit]

  final private[otter] case class Modify[A, B](self: Path[A], f: A => B, g: B => A) extends Path[B]

  final private[otter] case class Root[A](segment: Segment[A]) extends Path[A]

  final private[otter] case class Zip[A, B](left: Path[A], right: Path[B]) extends Path[(A, B)]

  given Invariant[Path] = new Invariant[Path]:
    override def imap[A, B](fa: Path[A])(f: A => B)(g: B => A): Path[B] = fa.imap(f)(g)
