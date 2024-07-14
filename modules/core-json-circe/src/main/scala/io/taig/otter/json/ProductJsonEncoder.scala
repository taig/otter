package io.taig.otter.json

import io.circe.Json
import io.taig.otter.*
import cats.syntax.all.*

object ProductJsonEncoder:
  def apply[A](schema: Product.Via[Json, A], a: A): Option[Vector[Json]] = schema match
    case Product.Combine(_, left, right) =>
      (ProductJsonEncoder(left, a._1), ProductJsonEncoder(right, a._2))
        .match
          case (Some(left), Some(right)) => left ++ right
          case (None, Some(right))       => Vector.fill(left.schemas.size.toInt)(Json.Null) ++ right
          case (Some(left), None)        => left ++ Vector.fill(right.schemas.size.toInt)(Json.Null)
          case (None, None)              => Vector.fill((left.schemas.size + right.schemas.size).toInt)(Json.Null)
        .some
    case Product.Empty(_)              => Vector.empty.some
    case Product.One(_, schema)        => Vector(JsonEncoder(schema, a)).some
    case Product.Optional(self)        => a.flatMap(ProductJsonEncoder(self, _))
    case Product.Transform(self, _, f) => ProductJsonEncoder(self, f(a))
