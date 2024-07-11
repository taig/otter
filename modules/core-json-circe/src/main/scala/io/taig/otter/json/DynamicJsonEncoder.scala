package io.taig.otter.json

import io.taig.otter.Plain.*
import io.taig.otter as Base
import io.circe.Json

object DynamicJsonEncoder:
  def apply[A](schema: Dynamic.Writer[Json, A], a: A): Json = schema match
    case Base.Dynamic.Root()                    => a
    case Base.Dynamic.Writer.Root()             => a
    case Base.Dynamic.Writer.Optional(self)     => optional(self, a)
    case Base.Dynamic.Optional(self)            => optional(self, a)
    case Base.Dynamic.Writer.Transform(self, f) => transform(self, f, a)
    case Base.Dynamic.Transform(self, _, f)     => transform(self, f, a)

  def optional[A](self: Dynamic.Writer[Json, A], a: Option[A]): Json =
    a.fold(Json.Null)(DynamicJsonEncoder(self, _))

  def transform[A, B](self: Dynamic.Writer[Json, A], f: B => A, b: B): Json =
    DynamicJsonEncoder(self, f(b))
