package io.taig.otter.http.codec

import cats.data.Chain
import io.taig.otter.codec.Encoder
import io.taig.otter.http.Queries

object QueriesDataEncoder extends Encoder[Queries, Queries.Data]:
  override def encode[A](queries: Queries[A], a: A): Queries.Data = queries match
    case Queries.Empty              => Chain.empty
    case Queries.Root(query)        => QueryDataEncoder.encode(query, a)
    case Queries.Modify(self, _, g) => encode(queries = self, g(a))
    case Queries.Optional(self)     => a.map(encode(queries = self, _)).getOrElse(Chain.empty)
    case Queries.Zip(left, right)   => encode(queries = left, a._1) ++ encode(queries = right, a._2)
