package io.taig.otter.json

import io.taig.otter.*
import io.circe.Json

object EnumerationJsonEncoder:
  def apply[A](schema: Enumeration.Writer.Via[Json, A], a: A): Json = schema match
    case Enumeration.Optional(self)                     => optional(self, a)
    case Enumeration.Required.Transform(self, _, f)     => transform(self, f, a)
    case Enumeration.Required.Writer.Root(_, schema, f) => root(schema, f, a)
    case Enumeration.Required.Writer.Transform(self, f) => transform(self, f, a)
    case Enumeration.Root(_, schema, mapping)           => root(schema, mapping.inj, a)
    case Enumeration.Transform(self, _, f)              => transform(self, f, a)
    case Enumeration.Writer.Optional(self)              => optional(self, a)
    case Enumeration.Writer.Transform(self, f)          => transform(self, f, a)

  def optional[A](self: Enumeration.Writer.Via[Json, A], a: Option[A]): Json =
    a.fold(Json.Null)(EnumerationJsonEncoder(self, _))

  def root[A, B](schema: Value.Writer.Via[Json, A], f: B => A, b: B): Json = JsonEncoder(schema, f(b))

  def transform[A, B](self: Enumeration.Writer.Via[Json, A], f: B => A, b: B): Json =
    EnumerationJsonEncoder(self, f(b))
