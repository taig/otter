package io.taig.otter.json.circe

import cats.syntax.all.*
import io.circe.Json
import io.taig.otter.Tuple
import cats.Id as Identity

object JsonTupleEncoder:
  def apply[A](schema: Tuple[Identity, ?, A], a: A): Option[Vector[Json]] = schema match
    case Tuple.Empty       => Vector.empty.some
    case Tuple.One(schema) => Vector(JsonEncoder(schema, a)).some
    case Tuple.Product(left, right) =>
      (apply(left, a._1), apply(right, a._2)) match
        case (Some(left), Some(right)) => Some(left ++ right)
        case (Some(left), None)        => Some(left ++ Vector.fill(right.size)(Json.Null))
        case (None, Some(right))       => Some(Vector.fill(left.size)(Json.Null) ++ right)
        case (None, None)              => None
