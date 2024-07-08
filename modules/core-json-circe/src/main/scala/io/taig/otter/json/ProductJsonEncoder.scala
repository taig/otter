package io.taig.otter.json

import io.circe.Json
import io.taig.otter as Base
import io.taig.otter.Plain.*
import cats.syntax.all.*

object ProductJsonEncoder:
  def apply[A](schema: Product.Writer[A], a: A): Option[Vector[Json]] = schema match
    case Base.Product.Combine(left, right)        => combine(left, right, a).some
    case Base.Product.Empty                       => Vector.empty.some
    case Base.Product.One(schema)                 => one(schema, a).some
    case Base.Product.Optional(self)              => optional(self, a)
    case Base.Product.Transform(self, _, f)       => transform(self, f, a)
    case Base.Product.Writer.Combine(left, right) => combine(left, right, a).some
    case Base.Product.Writer.One(schema)          => one(schema, a).some
    case Base.Product.Writer.Optional(self)       => optional(self, a)
    case Base.Product.Writer.Transform(self, f)   => transform(self, f, a)

  def one[A](schema: Schema.Writer[A], a: A): Vector[Json] = Vector(JsonEncoder(schema, a))

  def optional[A](self: Product.Writer[A], a: Option[A]): Option[Vector[Json]] = a.flatMap(apply(self, _))

  def transform[A, B](self: Product.Writer[A], f: B => A, b: B): Option[Vector[Json]] = apply(self, f(b))

  def combine[A, B](left: Product.Writer[A], right: Product.Writer[B], ab: (A, B)): Vector[Json] =
    (ProductJsonEncoder(left, ab._1), ProductJsonEncoder(right, ab._2)) match
      case (Some(left), Some(right)) => left ++ right
      case (None, Some(right))       => Vector.fill(left.schemas.size.toInt)(Json.Null) ++ right
      case (Some(left), None)        => left ++ Vector.fill(right.schemas.size.toInt)(Json.Null)
      case (None, None)              => Vector.fill((left.schemas.size + right.schemas.size).toInt)(Json.Null)
