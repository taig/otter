package io.taig.otter.component

import io.taig.otter.Comparison
import io.taig.otter.schema.FieldSchema
import io.taig.otter.schema.NullableSchema
import io.taig.otter.schema.RecordSchema

trait ComparisonComponent[Nullable[a] <: Value[a], Record[a] <: Value[a], Field[_], Key[_], Value[_]](using
    FieldSchema[Field, Key, Value],
    NullableSchema[Nullable, Value],
    RecordSchema[Record, Field]
) extends FieldComponent.Primitive.String[Field, Key, Value, Record],
      NullableComponent[Nullable, Value],
      PrimitiveComponent.Boolean[Value],
      RecordComponent[Record, Field]:
  def comparison[A](schema: => Value[A]): Record[Comparison[A]] =
    (field("reference", schema) :* field("exclusive", boolean.nullable(default = false))).to
