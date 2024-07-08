package io.taig.otter.http

import cats.data.Chain

sealed trait Queries[+F[+_], +G[+_], +A]:
  final def map[B](f: A => B): Queries[F, G, B] = Queries.Transform(this, f)
  def toQueries: Chain[F[Query[G, ?]]]
  def translate[H[+_]](fK: [A] => F[A] => H[A]): Queries[H, G, A]
  final def zip[F1[+a] >: F[a], G1[+a] >: G[a], B](queries: Queries[F1, G1, B]): Queries[F1, G1, (A, B)] =
    Queries.Combine(this, queries)

object Queries:
  final case class Combine[F[+_], G[+_], A, B](left: Queries[F, G, A], right: Queries[F, G, B])
      extends Queries[F, G, (A, B)]:
    override def toQueries: Chain[F[Query[G, ?]]] = left.toQueries ++ right.toQueries
    override def translate[H[+_]](fK: [A] => F[A] => H[A]): Queries[H, G, (A, B)] =
      copy(left = left.translate(fK), right = right.translate(fK))

  case object Empty extends Queries[Nothing, Nothing, Unit]:
    override def toQueries: Chain[Nothing] = Chain.empty
    override def translate[H[+_]](fK: [A] => Nothing => H[A]): Queries[H, Nothing, Unit] = this

  final case class One[F[+_], G[+_], A](query: F[Query[G, A]]) extends Queries[F, G, A]:
    override def toQueries: Chain[F[Query[G, A]]] = Chain.one(query)
    override def translate[H[+_]](fK: [A] => F[A] => H[A]): Queries[H, G, A] =
      copy(query = fK(query))

  final case class Transform[F[+_], G[+_], A, B](self: Queries[F, G, A], f: A => B) extends Queries[F, G, B]:
    export self.toQueries
    override def translate[H[+_]](fK: [A] => F[A] => H[A]): Queries[H, G, B] = copy(self = self.translate(fK))
