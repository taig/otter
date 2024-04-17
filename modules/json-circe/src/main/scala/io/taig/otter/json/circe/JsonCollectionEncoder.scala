package io.taig.otter.json.circe

import io.taig.otter.Collection
import cats.data.Chain
import io.taig.otter.Schema
import cats.syntax.all.*
import io.circe.Json

object JsonCollectionEncoder:
  def encode[A](schema: Collection[Schema[?], A], a: A): Option[Chain[Json]] = schema match
    case Collection.Optional(schema)          => a.flatMap(encode(schema, _))
    case Collection.Root(schema)              => a.map(JsonEncoder.encode(schema, _)).some
    case Collection.Validate(schema, _, _, g) => encode(schema, g(a))
