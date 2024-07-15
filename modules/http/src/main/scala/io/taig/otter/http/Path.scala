package io.taig.otter.http

import cats.data.Chain
import cats.syntax.all.*
import cats.Invariant

sealed trait Path[A] extends Product, Serializable:
  final def imap[B](f: A => B)(g: B => A): Path[B] = Path.Transform(this, f, g)
  def segments: Chain[Segment[?]]
  final def zip[B](path: Path[B]): Path[(A, B)] = Path.Combine(this, path)

object Path:
  final case class Combine[A, B](left: Path[A], right: Path[B]) extends Path[(A, B)]:
    override def segments: Chain[Segment[?]] = left.segments ++ right.segments

  case object Empty extends Path[Unit]:
    override def segments: Chain[Nothing] = Chain.empty

  final case class One[A](segment: Segment[A]) extends Path[A]:
    override def segments: Chain[Segment[A]] = Chain.one(segment)

  final case class Transform[A, B](self: Path[A], f: A => B, g: B => A) extends Path[B]:
    export self.segments

  given Invariant[Path] with
    override def imap[A, B](fa: Path[A])(f: A => B)(g: B => A): Path[B] = fa.imap(f)(g)
