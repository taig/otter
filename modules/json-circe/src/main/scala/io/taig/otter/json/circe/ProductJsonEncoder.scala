package io.taig.otter.json.circe

import io.circe.Json
import io.taig.otter as Base
import io.taig.otter.Plain.*
import cats.syntax.all.*

object ProductJsonEncoder:
  def apply[A](schema: Product.Writer[A], a: A): Option[Vector[Json]] = ???
  // def apply[A](schema: Tuple.Writer[A], a: A): Option[Vector[Json]] = schema match
  //   case Base.Tuple.Empty()                 => empty
  //   case Base.Tuple.Modify(self, _, f)      => modify(self, f, a)
  //   case Base.Tuple.One(schema)             => one(schema, a)
  //   case Base.Tuple.Optional(self)          => optional(self, a)
  //   case Base.Tuple.Zip(left, right)        => zip(left, right, a)
  //   case Base.Tuple.Writer.Empty()          => empty
  //   case Base.Tuple.Writer.Modify(self, f)  => modify(self, f, a)
  //   case Base.Tuple.Writer.One(schema)      => one(schema, a)
  //   case Base.Tuple.Writer.Optional(self)   => optional(self, a)
  //   case Base.Tuple.Writer.Zip(left, right) => zip(left, right, a)

  // val empty: Option[Vector[Json]] = Vector.empty.some

  // def modify[A, B](self: Tuple.Writer[A], f: B => A, b: B): Option[Vector[Json]] = apply(self, f(b))

  // def one[A](schema: Schema.Writer[A], a: A): Option[Vector[Json]] = Vector(JsonEncoder(schema, a)).some

  // def optional[A](self: Tuple.Writer[A], a: Option[A]): Option[Vector[Json]] = a.flatMap(apply(self, _))

  // def zip[A, B](left: Tuple.Writer[A], right: Schema.Writer[B], ab: (A, B)): Option[Vector[Json]] =
  //   (apply(left, ab._1), JsonEncoder(right, ab._2)) match
  //     case (Some(left), right) => Some(left :+ right)
  //     case (None, Json.Null)   => None
  //     case (None, right)       => Some(Vector.fill(left.schemas.size.toInt)(Json.Null) :+ right)
