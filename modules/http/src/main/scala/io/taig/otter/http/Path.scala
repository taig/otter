package io.taig.otter.http

import cats.data.Chain
import cats.syntax.all.*

sealed trait Path[+F[+_], +G[+_], +A] extends Product, Serializable:
  def toSegments: Chain[F[Segment[G, ?]]]
  def translate[H[+_]](fK: [A] => F[A] => H[A]): Path[H, G, A]
  final def zip[F1[+a] >: F[a], G1[+a] >: G[a], B](path: Path[F1, G1, B]): Path[F1, G1, (A, B)] =
    Path.Combine(this, path)

object Path:
  final case class Combine[F[+_], G[+_], A, B](left: Path[F, G, A], right: Path[F, G, B]) extends Path[F, G, (A, B)]:
    override def toSegments: Chain[F[Segment[G, ?]]] = left.toSegments ++ right.toSegments
    override def translate[H[+_]](fK: [A] => F[A] => H[A]): Path[H, G, (A, B)] =
      copy(left = left.translate(fK), right = right.translate(fK))

  final case class Dynamic[F[+_], G[+_], A](segment: F[Segment[G, A]]) extends Path[F, G, A]:
    override def toSegments: Chain[F[Segment[G, ?]]] = Chain.one(segment)
    def translate[H[+_]](fK: [A] => F[A] => H[A]): Path[H, G, A] = copy(segment = fK(segment))

  case object Empty extends Path[Nothing, Nothing, Unit]:
    override def toSegments: Chain[Nothing] = Chain.empty
    override def translate[H[+_]](fK: [A] => Nothing => H[A]): Path[Nothing, Nothing, Unit] = this

  final case class Static(name: String) extends Path[Nothing, Nothing, Unit]:
    override def toSegments: Chain[Nothing] = Chain.empty
    override def translate[H[+_]](fK: [A] => Nothing => H[A]): Path[Nothing, Nothing, Unit] = this
