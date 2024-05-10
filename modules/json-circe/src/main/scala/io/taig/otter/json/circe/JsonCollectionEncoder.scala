package io.taig.otter.json.circe

import io.taig.otter.Collection
import cats.data.Chain
import io.taig.otter.Fix
import cats.syntax.all.*
import io.circe.Json

object JsonCollectionEncoder:
  def apply[A, B](schema: Collection.Writer[JsonSchema.Writer[A], B], b: B): Option[Chain[Json]] = schema match
    case Collection.Writer.Root(schema)    => b.map(JsonEncoder(schema, _)).some
    case Collection.Writer.Modify(self, f) => apply(self, f(b))
    case Collection.Writer.Optional(self)  => b.flatMap(apply(self, _))
    case Collection(_, asWrite)            => apply(asWrite, b)
