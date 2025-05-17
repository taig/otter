package io.taig.otter.http.codec

import io.taig.otter.codec.Encoder
import io.taig.otter.http.Query
import io.taig.otter.http.Queries
import cats.syntax.all.*
import cats.data.Chain

object QueryDataEncoder extends Encoder[Query, Queries.Data]:
  override def encode[A](schema: Query[A], a: A): Queries.Data = schema match
    case Query.Modify(self, _, g) => encode(schema = self, g(a))
    case Query.Optional(self)     => a.map(encode(schema = self, _)).getOrElse(Chain.empty)
    case Query.Root(name, schema, explode, style, _) =>
      HttpQueryEncoder(explode, style).encode(schema = schema.value, a) match
        case Some(values) => Chain.fromSeq(values).map(value => (name, value.some))
        case None         => Chain.one((name, none))
