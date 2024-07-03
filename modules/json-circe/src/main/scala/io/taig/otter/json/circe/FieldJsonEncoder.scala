package io.taig.otter.json.circe

import io.taig.otter.Plain.*
import io.taig.otter as Base
import io.circe.Json
import io.taig.otter.StringEncoder

object FieldJsonEncoder:
  def apply[A](field: Field.Writer[A], a: A): (String, Json) = field match
    case Base.Field.Root(name, schema)        => name -> JsonEncoder(schema, a)
    case Base.Field.Writer.Root(name, schema) => name -> JsonEncoder(schema, a)
