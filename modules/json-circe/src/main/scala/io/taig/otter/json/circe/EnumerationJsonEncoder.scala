package io.taig.otter.json.circe

import io.taig.otter.Plain.*
import io.taig.otter as Base
import io.circe.Json

object EnumerationJsonEncoder:
  def apply[A](schema: Enumeration.Writer[A], a: A): Json = schema match
    case Base.Enumeration.Optional(self)                     => optional(self, a)
    case Base.Enumeration.Required.Transform(self, _, f)     => transform(self, f, a)
    case Base.Enumeration.Required.Writer.Root(schema, f)    => root(schema, f, a)
    case Base.Enumeration.Required.Writer.Transform(self, f) => transform(self, f, a)
    case Base.Enumeration.Root(self, mapping)                => root(self, mapping.inj, a)
    case Base.Enumeration.Transform(self, _, f)              => transform(self, f, a)
    case Base.Enumeration.Writer.Optional(self)              => optional(self, a)
    case Base.Enumeration.Writer.Transform(self, f)          => transform(self, f, a)

  def optional[A](self: Enumeration.Writer[A], a: Option[A]): Json =
    a.fold(Json.Null)(EnumerationJsonEncoder(self, _))

  def root[A, B](schema: Value.Writer[A], f: B => A, b: B): Json = JsonEncoder(schema, f(b))

  def transform[A, B](self: Enumeration.Writer[A], f: B => A, b: B): Json =
    EnumerationJsonEncoder(self, f(b))
