package io.taig.otter.http

import cats.data.Chain
import io.taig.otter.Reference

type Path[+F[_], A] = Path.Read[F, A] & Path.Write[F, A]

object Path:
  sealed trait Read[+F[_], +A]:
    def segments: Chain[Reference[F, ?]]

  sealed trait Write[+F[_], -A]:
    def segments: Chain[Reference[F, ?]]

  case object Empty extends Path.Read[Nothing, Unit], Path.Write[Nothing, Unit]:
    override def segments: Chain[Nothing] = Chain.empty

  final case class Product[F[_], A, B](left: Path[F, A], right: Path[F, B])
      extends Path.Read[F, (A, B)],
        Path.Write[F, (A, B)]:
    override def segments: Chain[Reference[F, ?]] = left.segments ++ right.segments

  final case class Root[F[_], A](segment: Reference[F, A]) extends Path.Read[F, A], Path.Write[F, A]:
    override def segments: Chain[Reference[F, ?]] = Chain.one(segment)
