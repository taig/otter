package io.taig.otter.json.circe

import io.taig.otter.Tuple
import cats.data.Chain
import cats.syntax.all.*
import io.circe.Json
import io.taig.otter.Schema

object JsonTupleEncoder:
  def encode[A](schema: Tuple[Schema[?], A], value: A): Option[Chain[Json]] = schema match
    case Tuple.Empty => Chain.empty.some
    // case Tuple.Modify(schema, _, g) => encode(schema, g(value))
    case Tuple.One(schema)      => Chain.one(JsonEncoder.encode(schema, value)).some
    case Tuple.Optional(schema) => value.flatMap(encode(schema, _))
    case Tuple.Product(left, right) =>
      (encode(left, value._1), encode(right, value._2)) match
        case (Some(left), Some(right)) => (left ++ right).some
        case (Some(left), None)        => (left ++ Chain.fromSeq(Seq.fill(right.size)(Json.Null))).some
        case (None, Some(right))       => (Chain.fromSeq(Seq.fill(left.size)(Json.Null)) ++ right).some
        case (None, None)              => none
