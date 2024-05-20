package io.taig.otter.json.circe

import io.circe.Json
import cats.Id as Identity
import io.taig.otter as Base

object JsonTupleEncoder:
  def apply[A](data: Base.Tuple[Identity, A], a: A): Vector[Json] = data match
    case Base.Tuple.Root(schema) => Vector(JsonEncoder(schema, a))
