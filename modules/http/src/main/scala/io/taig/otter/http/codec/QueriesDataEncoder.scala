package io.taig.otter.http.codec

import cats.data.Chain
import io.taig.otter.codec.Encoder
import io.taig.otter.http.Queries

object QueriesDataEncoder extends Encoder[Queries, Queries.Data]:
  override def encode[A](queries: Queries[A], a: A): Queries.Data =
    encode(queries = queries.value, a)

  def encode[A](queries: Queries.Value[A], a: A): Queries.Data = queries match
    case Queries.Value.Empty              => Chain.empty
    case Queries.Value.Root(query)        => QueryDataEncoder.encode(query, a)
    case Queries.Value.Modify(self, _, g) => encode(queries = self, g(a))
    case Queries.Value.Optional(self)     => a.map(encode(queries = self, _)).getOrElse(Chain.empty)
    case Queries.Value.Zip(left, right)   => encode(queries = left, a._1) ++ encode(queries = right, a._2)
