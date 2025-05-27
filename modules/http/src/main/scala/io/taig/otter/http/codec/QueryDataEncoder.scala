package io.taig.otter.http.codec

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.codec.Encoder
import io.taig.otter.http.Queries
import io.taig.otter.http.Query

object QueryDataEncoder extends Encoder[Query, Queries.Data]:
  override def encode[A](schema: Query[A], a: A): Queries.Data = encode(schema = schema.self, a)

  def encode[A](schema: Query.Value[A], a: A): Queries.Data = schema match
    case Query.Value.Modify(self, _, g) => encode(schema = self, g(a))
    case Query.Value.Optional(self)     => a.map(encode(schema = self, _)).getOrElse(Chain.empty)
    case Query.Value.Root(name, schema, explode, style) =>
      QuerySchemaEncoder(explode, style).encode(schema = schema.value, a) match
        case Some(values) => Chain.fromSeq(values).map(value => (name, value.some))
        case None         => Chain.one((name, none))
