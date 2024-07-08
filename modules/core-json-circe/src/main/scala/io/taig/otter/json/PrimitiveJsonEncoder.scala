package io.taig.otter.json

import io.circe.Json
import io.taig.otter as Base
import io.taig.otter.Plain.*

object PrimitiveJsonEncoder:
  def apply[A](schema: Primitive.Writer[A], a: A): Json = schema match
    case Base.Primitive.Optional(self)                     => optional(self, a)
    case Base.Primitive.Required.Root(tpe)                 => TypeJsonEncoder(tpe, a)
    case Base.Primitive.Required.Transform(self, _, f)     => transform(self, f, a)
    case Base.Primitive.Required.Writer.Transform(self, f) => transform(self, f, a)
    case Base.Primitive.Transform(self, _, f)              => transform(self, f, a)
    case Base.Primitive.Writer.Transform(self, f)          => transform(self, f, a)
    case Base.Primitive.Writer.Optional(self)              => optional(self, a)

  def optional[A](self: Primitive.Writer[A], a: Option[A]): Json = a.map(apply(self, _)).getOrElse(Json.Null)

  def transform[A, B](self: Primitive.Writer[A], f: B => A, b: B): Json = apply(self, f(b))
