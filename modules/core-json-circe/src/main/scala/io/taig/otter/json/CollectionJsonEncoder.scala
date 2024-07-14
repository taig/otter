package io.taig.otter.json

import io.circe.Json
import io.taig.otter.*
import cats.syntax.all.*

object CollectionJsonEncoder:
  def apply[A](schema: Collection.Via[Json, A], a: A): Option[Vector[Json]] = schema match
    case Collection.Optional(self)        => a.flatMap(apply(self, _))
    case Collection.Root(_, schema)       => a.map(JsonEncoder(schema, _)).some
    case Collection.Transform(self, _, f) => CollectionJsonEncoder(self, f(a))
