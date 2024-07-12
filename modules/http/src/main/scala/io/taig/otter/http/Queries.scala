package io.taig.otter.http

import cats.data.Chain

sealed trait Queries[+A]:
  final def map[B](f: A => B): Queries[B] = Queries.Transform(this, f)
  def toQueries: Chain[Query[?]]
  final def zip[B](queries: Queries[B]): Queries[(A, B)] = Queries.Combine(this, queries)

object Queries:
  final case class Combine[A, B](left: Queries[A], right: Queries[B]) extends Queries[(A, B)]:
    override def toQueries: Chain[Query[?]] = left.toQueries ++ right.toQueries

  case object Empty extends Queries[Unit]:
    override def toQueries: Chain[Nothing] = Chain.empty

  final case class One[A](query: Query[A]) extends Queries[A]:
    override def toQueries: Chain[Query[A]] = Chain.one(query)

  final case class Transform[A, B](self: Queries[A], f: A => B) extends Queries[B]:
    export self.toQueries
