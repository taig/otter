package io.taig.otter.json.circe

import io.taig.otter.Collection
import cats.data.Chain
import io.taig.otter.Schema
import io.taig.otter.Fix
import cats.syntax.all.*
import io.circe.Json

object JsonCollectionEncoder:
  def apply[A, B](schema: Collection.Write[Schema.Write.Identity[A], B], b: B): Option[Chain[Json]] = schema match
    case Collection.Write.Root(schema)    => b.map(JsonEncoder(schema.unfix, _)).some
    case Collection.Write.Modify(self, f) => apply(self, f(b))
    case Collection.Write.Optional(self)  => b.flatMap(apply(self, _))
    case Collection(_, asWrite)           => apply(asWrite, b)
