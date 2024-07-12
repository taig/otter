package io.taig.otter.json

import io.circe.Json
import io.taig.otter.*

object PrimitiveJsonEncoder:
  def apply[A](schema: Primitive.Writer[A], a: A): Json = schema match
    case Primitive.Optional(self)                     => optional(self, a)
    case Primitive.Required.Root(_, tpe)              => TypeJsonEncoder(tpe, a)
    case Primitive.Required.Transform(self, _, f)     => transform(self, f, a)
    case Primitive.Required.Writer.Transform(self, f) => transform(self, f, a)
    case Primitive.Transform(self, _, f)              => transform(self, f, a)
    case Primitive.Writer.Transform(self, f)          => transform(self, f, a)
    case Primitive.Writer.Optional(self)              => optional(self, a)

  def optional[A](self: Primitive.Writer[A], a: Option[A]): Json = a.map(apply(self, _)).getOrElse(Json.Null)

  def transform[A, B](self: Primitive.Writer[A], f: B => A, b: B): Json = apply(self, f(b))
