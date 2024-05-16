package io.taig.otter.json.circe

import cats.data.Chain
import cats.syntax.all.*
import io.circe.Json
import io.taig.otter as Base
import cats.Id

object JsonTupleEncoder:
  def apply[A](schema: Base.Tuple[Id, ?, A], a: A): Option[Chain[Json]] = schema match
    case Base.Tuple.Empty       => Chain.empty.some
    case Base.Tuple.One(schema) => Chain.one(JsonEncoder(schema, a)).some
