package io.taig.otter.json

import io.circe.Json
import io.taig.otter.*
import cats.syntax.all.*

object ProductJsonEncoder:
  def apply[A](schema: Product.Writer.Via[Json, A], a: A): Option[Vector[Json]] = schema match
    case Product.Combine(_, left, right)        => combine(left, right, a).some
    case Product.Empty(_)                       => Vector.empty.some
    case Product.One(_, schema)                 => one(schema, a).some
    case Product.Optional(self)                 => optional(self, a)
    case Product.Transform(self, _, f)          => transform(self, f, a)
    case Product.Writer.Combine(_, left, right) => combine(left, right, a).some
    case Product.Writer.One(_, schema)          => one(schema, a).some
    case Product.Writer.Optional(self)          => optional(self, a)
    case Product.Writer.Transform(self, f)      => transform(self, f, a)

  def one[A](schema: Schema.Writer.Via[Json, A], a: A): Vector[Json] = Vector(JsonEncoder(schema, a))

  def optional[A](self: Product.Writer.Via[Json, A], a: Option[A]): Option[Vector[Json]] = a.flatMap(apply(self, _))

  def transform[A, B](self: Product.Writer.Via[Json, A], f: B => A, b: B): Option[Vector[Json]] = apply(self, f(b))

  def combine[A, B](left: Product.Writer.Via[Json, A], right: Product.Writer.Via[Json, B], ab: (A, B)): Vector[Json] =
    (ProductJsonEncoder(left, ab._1), ProductJsonEncoder(right, ab._2)) match
      case (Some(left), Some(right)) => left ++ right
      case (None, Some(right))       => Vector.fill(left.schemas.size.toInt)(Json.Null) ++ right
      case (Some(left), None)        => left ++ Vector.fill(right.schemas.size.toInt)(Json.Null)
      case (None, None)              => Vector.fill((left.schemas.size + right.schemas.size).toInt)(Json.Null)
