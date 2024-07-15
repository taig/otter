package io.taig.otter.http

import cats.data.Chain

object QueriesEncoder:
  def apply[A](queries: Queries[A], a: A): Chain[(String, Option[String])] = queries match
    case Queries.Combine(left, right)  => QueriesEncoder(left, a._1) ++ QueriesEncoder(right, a._2)
    case Queries.Empty                 => Chain.empty
    case Queries.One(query)            => Chain.one(ParameterEncoder(query, a))
    case Queries.Transform(self, _, f) => QueriesEncoder(self, f(a))
