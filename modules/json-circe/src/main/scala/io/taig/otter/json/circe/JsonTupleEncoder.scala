package io.taig.otter.json.circe

import cats.data.Chain
import cats.syntax.all.*
import io.circe.Json
import io.taig.otter.Tuple
import io.taig.otter.Plain

object JsonTupleEncoder:
  def apply[A, B](schema: Plain.Tuple.Writer[A, B], b: B): Option[Chain[Json]] = schema match
    case Tuple.Empty                    => Chain.empty.some
    case Tuple.One(schema, unwrap)      => Chain(JsonEncoder(unwrap(schema), b)).some
    case s @ Tuple.Product(left, right) =>
      // TODO
      apply(left, b._1) |+| apply(s.unwrap, b._2)
    case Tuple.Optional(self) => b.flatMap(apply(self, _))
