package io.taig.otter.json.circe

import io.taig.otter.Tuple
import cats.data.Chain
import cats.syntax.all.*
import io.circe.Json

object JsonTupleEncoder:
  def apply[A](schema: Tuple[?, A], value: A): Option[Chain[Json]] = schema match
    case Tuple.Empty(_)             => Chain.empty.some
    case Tuple.Modify(schema, _, g) => apply(schema, g(value))
    case Tuple.One(_, schema)       => Chain.one(JsonEncoder(schema, value)).some
    case Tuple.Optional(schema)     => value.flatMap(apply(schema, _))
    case Tuple.Product(_, left, right) =>
      (apply(left, value._1), apply(right, value._2)) match
        case (Some(left), Some(right)) => (left ++ right).some
        case (Some(left), None)        => (left ++ Chain.fromSeq(Seq.fill(right.size)(Json.Null))).some
        case (None, Some(right))       => (Chain.fromSeq(Seq.fill(left.size)(Json.Null)) ++ right).some
        case (None, None)              => none
