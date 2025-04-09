package io.taig.otter.http

import io.taig.otter.Encoder
import org.http4s.Query as Http4sQuery

object Http4sQueriesEncoder extends Encoder[Queries, Http4sQuery]:
  override def apply[A](queries: Queries[A], a: A): Http4sQuery = queries match
    case Queries.Empty              => Http4sQuery.Empty
    case Queries.Root(query)        => Http4sQueryEncoder(query, a)
    case Queries.Modify(self, _, g) => apply(queries = self, g(a))
    case Queries.Optional(self)     => a.fold(Http4sQuery.empty)(apply(queries = self, _))
    case Queries.Zip(left, right)   => apply(queries = left, a._1) ++ apply(queries = right, a._2).toVector
