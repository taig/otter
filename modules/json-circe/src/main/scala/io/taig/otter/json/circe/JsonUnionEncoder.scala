package io.taig.otter.json.circe

import io.taig.otter.+
import io.taig.otter.Plain.*
import io.taig.otter as Base
import io.circe.Json

object JsonUnionEncoder:
  def apply[A](schema: Union.Writer[A], a: A): Json = schema match
    case Base.Union.Modify(self, _, f)         => modify(self, f, a)
    case Base.Union.One(schema)                => one(schema, a)
    case Base.Union.Optional(self)             => optional(self, a)
    case Base.Union.OrElse(left, right)        => orElse(left, right, a)
    case Base.Union.Writer.Modify(self, f)     => modify(self, f, a)
    case Base.Union.Writer.One(schema)         => one(schema, a)
    case Base.Union.Writer.Optional(self)      => optional(self, a)
    case Base.Union.Writer.OrElse(left, right) => orElse(left, right, a)

  def modify[A, B](self: Union.Writer[A], f: B => A, b: B): Json = apply(self, f(b))

  def one[A](schema: Schema.Writer[A], a: A): Json = JsonEncoder(schema, a)

  def optional[A](self: Union.Writer[A], a: Option[A]): Json = a.map(apply(self, _)).getOrElse(Json.Null)

  def orElse[A, B](left: Union.Writer[A], right: Schema.Writer[B], ab: A + B): Json =
    ab.fold(apply(left, _), JsonEncoder(right, _))
