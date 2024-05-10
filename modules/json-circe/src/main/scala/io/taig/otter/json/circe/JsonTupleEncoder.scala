package io.taig.otter.json.circe

import io.taig.otter.Tuple
import cats.data.Chain
import cats.syntax.all.*
import io.circe.Json
import io.taig.otter.Schema

object JsonTupleEncoder:
  def apply[A, B](schema: Tuple.Writer[Schema.Writer.Identity[A], B], b: B): Option[Chain[Json]] = schema match
    case Tuple.Writer.Empty | Tuple.Empty  => Chain.empty.some
    case Tuple.One(schema)                 => Chain.one(JsonEncoder(schema.unfix, b)).some
    case Tuple.Writer.One(schema)          => Chain.one(JsonEncoder(schema.unfix, b)).some
    case Tuple.Optional(schema)            => b.flatMap(apply(schema, _))
    case Tuple.Writer.Optional(schema)     => b.flatMap(apply(schema, _))
    case Tuple.Validate(schema, _, f)      => apply(schema, f(b))
    case Tuple.Writer.Modify(schema, f)    => apply(schema, f(b))
    case Tuple.Product(left, right)        => apply(left, right, b)
    case Tuple.Writer.Product(left, right) => apply(left, right, b)

  def apply[A, B, C](
      left: Tuple.Writer[Schema.Writer.Identity[A], B],
      right: Tuple.Writer[Schema.Writer.Identity[A], C],
      ab: (B, C)
  ): Option[Chain[Json]] = (apply(left, ab._1), apply(right, ab._2)) match
    case (Some(left), Some(right)) => (left ++ right).some
    case (Some(left), None)        => (left ++ right.schemas.as(Json.Null)).some
    case (None, Some(right))       => (left.schemas.as(Json.Null) ++ right).some
    case (None, None)              => none
