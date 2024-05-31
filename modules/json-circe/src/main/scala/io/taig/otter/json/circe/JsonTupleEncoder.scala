package io.taig.otter.json.circe

import io.circe.Json
import io.taig.otter as Base
import io.taig.otter.Plain.*

object JsonTupleEncoder:
  def apply[A](data: Base.Tuple[Base.Writer[AsSchema, Base.Optional, Base.Schema, ?, ?], A], a: A): Vector[Json] =
    data match
      case Base.Tuple.Empty                => Vector.empty
      case Base.Tuple.One(schema)          => Vector(JsonEncoder(schema, a))
      case Base.Tuple.Product(left, right) => apply(left, a._1) ++ apply(right, a._2)
