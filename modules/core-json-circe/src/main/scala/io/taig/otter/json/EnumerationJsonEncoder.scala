package io.taig.otter.json

import io.taig.otter.*
import io.circe.Json

object EnumerationJsonEncoder:
  def apply[A](schema: Enumeration[?, A], a: A): Json = schema match
    case Enumeration.Optional(self)                    => a.fold(Json.Null)(EnumerationJsonEncoder(self, _))
    case Enumeration.Required.Transform(self, _, f)    => EnumerationJsonEncoder(self, f(a))
    case Enumeration.Root(_, schema, mapping)          => JsonEncoder(schema, mapping.inj(a))
    case Enumeration.Required.Root(_, schema, mapping) => JsonEncoder(schema, mapping.inj(a))
    case Enumeration.Transform(self, _, f)             => EnumerationJsonEncoder(self, f(a))
