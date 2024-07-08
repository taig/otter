package io.taig.otter.http

import cats.data.Chain
import cats.Functor

sealed trait Path[+F[+_], +A] extends Product, Serializable:
  def toSegments: Chain[Segment[F, ?]]
  def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Path[G, A]

object Path:
  final case class Combine[F[+_], A, B](left: Path[F, A], right: Path[F, B]) extends Path[F, (A, B)]:
    override def toSegments: Chain[Segment[F, ?]] = left.toSegments ++ right.toSegments
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Path[G, (A, B)] =
      copy(left = left.translate(fK), right = right.translate(fK))

  case object Empty extends Path[Nothing, Unit]:
    override def toSegments: Chain[Nothing] = Chain.empty
    override def translate[G[+_]: Functor](fK: [A] => Nothing => G[A]): Path[Nothing, Unit] = this

  final case class One[F[+_], A](segment: Segment[F, A]) extends Path[F, A]:
    override def toSegments: Chain[Segment[F, ?]] = Chain.one(segment)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Path[G, A] =
      copy(segment = segment.translate(fK))
