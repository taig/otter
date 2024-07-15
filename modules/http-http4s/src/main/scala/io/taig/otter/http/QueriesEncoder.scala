package io.taig.otter.http

object QueriesEncoder:
  def apply[A](queries: Queries[A], a: A): List[(String, Option[String])] = queries match
    case Queries.Combine(left, right)  => QueriesEncoder(left, a._1) ++ QueriesEncoder(right, a._2)
    case Queries.Empty                 => List.empty
    case Queries.One(query)            => List(QueryEncoder(query, a))
    case Queries.Transform(self, _, f) => QueriesEncoder(self, f(a))
