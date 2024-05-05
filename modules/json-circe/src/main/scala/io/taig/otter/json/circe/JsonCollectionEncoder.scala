package io.taig.otter.json.circe

import io.taig.otter.Collection
import cats.data.Chain
import io.taig.otter.Schema
import cats.syntax.all.*
import io.circe.Json

object JsonCollectionEncoder:
  def encode[A](schema: Collection.Write[Schema[?], A], a: A): Option[Chain[Json]] = schema match
    case Collection.Write.Root(schema)     => a.map(JsonEncoder(schema, _)).some
    case Collection.Write.Modify(self, f)  => encode(self, f(a))
    case Collection.Write.Optional(schema) => a.flatMap(encode(schema, _))
    case Collection(_, asWrite)            => encode(asWrite, a)
