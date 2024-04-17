package io.taig.otter.json.circe

import io.taig.otter.Tuple
import cats.data.Chain
import cats.syntax.all.*
import io.circe.Json
import io.taig.otter.Schema

object JsonTupleEncoder:
  def encode[A](schema: Tuple[Schema[?], A], a: A): Option[Chain[Json]] = schema match
    case Tuple.Empty                     => Chain.empty.some
    case Tuple.Validate(schema, _, _, g) => encode(schema, g(a))
    case Tuple.One(schema)               => Chain.one(JsonEncoder.encode(schema, a)).some
    case Tuple.Optional(schema)          => a.flatMap(encode(schema, _))
    case Tuple.Product(left, right) =>
      (encode(left, a._1), encode(right, a._2)) match
        case (Some(left), Some(right)) => (left ++ right).some
        case (Some(left), None)        => (left ++ Chain.fromSeq(Seq.fill(right.size)(Json.Null))).some
        case (None, Some(right))       => (Chain.fromSeq(Seq.fill(left.size)(Json.Null)) ++ right).some
        case (None, None)              => none
