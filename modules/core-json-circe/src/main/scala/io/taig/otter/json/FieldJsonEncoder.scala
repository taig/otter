package io.taig.otter.json

import io.taig.otter.Plain.*
import io.taig.otter as Base
import io.circe.Json
import cats.syntax.all.*

object FieldJsonEncoder:
  def apply[A](field: Field.Writer[A], parent: Record.Null, a: A): Option[(String, Json)] = field match
    case Base.Field.Root(name, nulls, schema)        => root(name, schema, parent, nulls, a)
    case Base.Field.Writer.Root(name, nulls, schema) => root(name, schema, parent, nulls, a)

  def root[A](
      name: String,
      schema: Schema.Writer[A],
      parent: Record.Null,
      nulls: Field.Null,
      a: A
  ): Option[(String, Json)] =
    val hide = (parent, nulls) match
      case (Record.Null.Hide, Field.Null.Inherit) => true
      case (Record.Null.Show, Field.Null.Inherit) => false
      case (_, Field.Null.Hide)                   => true
      case (_, Field.Null.Show)                   => false

    Some(JsonEncoder(schema, a)).filterNot(json => hide && json.isNull).tupleLeft(name)
