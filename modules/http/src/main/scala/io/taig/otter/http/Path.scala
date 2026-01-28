package io.taig.otter.http

import cats.data.Chain
import io.taig.otter.Reference
import cats.Apply
import cats.ContravariantSemigroupal
import cats.InvariantSemigroupal
import io.taig.otter.http.operation.PathOperation

sealed abstract class Path[A] extends Path.Read[A], Path.Write[A]:
  override def segments: Chain[Reference[Http.Segment, ?]]

object Path:
  sealed trait Read[+A]:
    def segments: Chain[Reference[Http.Segment.Read, ?]]

  object Read:
    final case class Modify[A, B](self: Path.Read[A], f: A => B) extends Path.Read[B]:
      export self.segments

    final case class Product[A, B](left: Path.Read[A], right: Path.Read[B]) extends Path.Read[(A, B)]:
      override def segments: Chain[Reference[Http.Segment.Read, ?]] = left.segments ++ right.segments

    given Apply[Path.Read]:
      override def ap[A, B](ff: Path.Read[A => B])(fa: Path.Read[A]): Path.Read[B] =
        map(Product(ff, fa))(_ apply _)

      override def map[A, B](self: Path.Read[A])(f: A => B): Path.Read[B] = Modify(self, f)

  sealed trait Write[-A]:
    def segments: Chain[Reference[Http.Segment.Write, ?]]

  object Write:
    final case class Modify[A, B](self: Path.Write[A], f: B => A) extends Path.Write[B]:
      export self.segments

    final case class Product[A, B](left: Path.Write[A], right: Path.Write[B]) extends Path.Write[(A, B)]:
      override def segments: Chain[Reference[Http.Segment.Write, ?]] =
        left.segments ++ right.segments

    given ContravariantSemigroupal[Path.Write]:
      override def product[A, B](fa: Path.Write[A], fb: Path.Write[B]): Path.Write[(A, B)] = Product(fa, fb)

      override def contramap[A, B](self: Path.Write[A])(f: B => A): Path.Write[B] = Modify(self, f)

  case object Empty extends Path[Unit]:
    override def segments: Chain[Nothing] = Chain.empty

  final case class Modify[A, B](self: Path[A], f: A => B, g: B => A) extends Path[B]:
    override def segments: Chain[Reference[Http.Segment, ?]] = self.segments

  final case class Product[A, B](left: Path[A], right: Path[B]) extends Path[(A, B)]:
    override def segments: Chain[Reference[Http.Segment, ?]] = left.segments ++ right.segments

  final case class Root[A](segment: Reference[Http.Segment, A]) extends Path[A]:
    override def segments: Chain[Reference[Http.Segment, ?]] = Chain.one(segment)

  given InvariantSemigroupal[Path]:
    override def imap[A, B](self: Path[A])(f: A => B)(g: B => A): Path[B] = Modify(self, f, g)

    override def product[A, B](fa: Path[A], fb: Path[B]): Path[(A, B)] = Product(fa, fb)

  given PathOperation[Path] = ???
