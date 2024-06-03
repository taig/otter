package io.taig.otter.json.circe

import io.taig.otter as Base
import io.taig.otter.Plain.*
import io.circe.Json
import cats.syntax.all.*

object JsonCollectionEncoder:
  def apply[A](schema: Collection.Writer[A], a: A): Option[Vector[Json]] = schema match
    case Base.Collection.Modify(self, _, f) => apply(self, f(a))
    case Base.Collection.Optional(self)     => a.flatMap(apply(self, _))
    case Base.Collection.Root(schema)       => a.map(JsonEncoder(schema, _)).some
