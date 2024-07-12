package io.taig.otter.json

import io.taig.otter.*
import io.circe.Json
import cats.syntax.all.*

object FieldJsonEncoder:
  def apply[A](field: Field.Writer.Via[Json, A], parent: Null, a: A): Option[(String, Json)] = field match
    case Field.Root(_, name, schema)        => root(name, schema, parent, a)
    case Field.Writer.Root(_, name, schema) => root(name, schema, parent, a)

  def root[A](
      name: String,
      schema: Schema.Writer.Via[Json, A],
      parent: Null,
      a: A
  ): Option[(String, Json)] =
    // TODO
    // val hide = (parent, nulls) match
    //   case (Null.Hide, Field.Null.Inherit) => true
    //   case (Null.Show, Field.Null.Inherit) => false
    //   case (_, Field.Null.Hide)                   => true
    //   case (_, Field.Null.Show)                   => false
    val hide = false

    Some(JsonEncoder(schema, a)).filterNot(json => hide && json.isNull).tupleLeft(name)
