package io.taig.otter.json.circe

import cats.syntax.all.*
import io.circe.Json
import io.taig.otter.Tuple
import cats.Id as Identity
import io.taig.otter.Tuple.Empty
import io.taig.otter.Schema

object JsonTupleEncoder:
  def apply[A](schema: Tuple.Writer[Identity, ?, A], a: A): Option[Vector[Json]] = schema match
    case Tuple.Empty                       => Vector.empty.some
    case Tuple.Modify(self, _, f)          => modify(self, f, a)
    case Tuple.One(schema)                 => one(schema, a)
    case Tuple.Optional(self)              => optional(self, a)
    case Tuple.Writer.Empty                => Vector.empty.some
    case Tuple.Writer.Modify(self, f)      => modify(self, f, a)
    case Tuple.Writer.One(schema)          => one(schema, a)
    case Tuple.Writer.Optional(self)       => optional(self, a)
    case Tuple.Writer.Product(left, right) => product(left, right, a)
    case Tuple.Product(left, right)        => product(left, right, a)

  def modify[A, B](self: Tuple.Writer[Identity, ?, A], f: B => A, b: B): Option[Vector[Json]] = apply(self, f(b))

  def one[A](schema: Schema.Writer[Identity, ?, A], a: A): Option[Vector[Json]] =
    Vector(JsonEncoder(schema, a)).some

  def optional[A](self: Tuple.Writer[Identity, ?, A], a: Option[A]): Option[Vector[Json]] = a.flatMap(apply(self, _))

  def product[A, B](
      left: Tuple.Writer[Identity, ?, A],
      right: Tuple.Writer[Identity, ?, B],
      ab: (A, B)
  ): Option[Vector[Json]] = (apply(left, ab._1), apply(right, ab._2)) match
    case (Some(left), Some(right)) => Some(left ++ right)
    case (Some(left), None)        => Some(left ++ Vector.fill(right.size)(Json.Null))
    case (None, Some(right))       => Some(Vector.fill(left.size)(Json.Null) ++ right)
    case (None, None)              => None
