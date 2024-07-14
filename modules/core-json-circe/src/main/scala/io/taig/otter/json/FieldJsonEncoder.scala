package io.taig.otter.json

import io.taig.otter.*
import io.circe.Json
import cats.syntax.all.*
import io.taig.otter.Keys.*

object FieldJsonEncoder:
  def apply[A](field: Field.Writer.Via[Json, A], parent: Null, a: A): Option[(String, Json)] =
    val hideNulls = (parent, field.metadata(nulls)) match
      case (nulls, None)    => nulls === Null.Hide
      case (_, Some(nulls)) => nulls === Null.Hide

    field match
      case Field.Root(_, name, schema)        => root(name, schema, hideNulls, a)
      case Field.Writer.Root(_, name, schema) => root(name, schema, hideNulls, a)

  def root[A](name: String, schema: Schema.Writer.Via[Json, A], hideNulls: Boolean, a: A): Option[(String, Json)] =
    Some(JsonEncoder(schema, a)).filterNot(json => hideNulls && json.isNull).tupleLeft(name)
