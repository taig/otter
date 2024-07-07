package io.taig.otter.http

import cats.data.Chain

sealed trait Path[+F[+_], +A] extends Product, Serializable:
  def toSegments: Chain[Segment[F, ?]]

object Path:
  final case class Combine[F[+_], A, B](left: Path[F, A], right: Path[F, B]) extends Path[F, (A, B)]:
    override def toSegments: Chain[Segment[F, ?]] = left.toSegments ++ right.toSegments

  case object Empty extends Path[Nothing, Unit]:
    override def toSegments: Chain[Nothing] = Chain.empty

  final case class One[F[+_], A](segment: Segment[F, A]) extends Path[F, A]:
    override def toSegments: Chain[Segment[F, ?]] = Chain.one(segment)
