package io.taig.otter.json

import io.taig.otter.*
import io.circe.Json
import cats.syntax.all.*
import io.taig.otter.Keys.*

object FieldJsonEncoder:
  def apply[A](field: Field[?, A], parent: Null, a: A): Option[(String, Json)] =
    val hideNull = (parent, field.metadata(nulls)) match
      case (nulls, None)    => nulls === Null.Hide
      case (_, Some(nulls)) => nulls === Null.Hide

    FieldJsonEncoder(field, hideNull, a)

  def apply[A](field: Field[?, A], hideNull: Boolean, a: A): Option[(String, Json)] =
    field match
      case Field.Root(_, name, schema) =>
        Some(JsonEncoder(schema, a)).filterNot(json => hideNull && json.isNull).tupleLeft(name)
      case Field.Transform(self, _, f) => FieldJsonEncoder(self, hideNull, f(a))
