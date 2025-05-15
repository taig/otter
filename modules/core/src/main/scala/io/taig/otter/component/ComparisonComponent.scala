package io.taig.otter.component

import io.taig.otter.schema.RecordSchema
import io.taig.otter.Comparison

trait ComparisonComponent[Nullable[a] <: Value[a], Record[_], Field[_], Key[_], Value[_]](using
    RecordSchema[Record, Field]
) extends FieldComponent.Primitive.String[Field, Key, Value, Record],
      NullableComponent[Nullable, Value],
      PrimitiveComponent.Boolean[Value],
      RecordComponent[Record, Field]:
  def comparison[A](schema: => Value[A]): Record[Comparison[A]] =
    // val a = field("reference", schema) :* field("exclusive", nullable(boolean, default = false))
    // val b = field("reference", schema) *: field("exclusive", nullable(boolean, default = false))
    // val c = a :* field("reference", schema)
    // (field("reference", schema) :* field("exclusive", nullable(boolean, default = false))).to
    ???
