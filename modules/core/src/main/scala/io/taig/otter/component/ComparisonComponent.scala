package io.taig.otter.component

import io.taig.otter.Comparison
import io.taig.otter.operation.FieldSchemaInvariant
import io.taig.otter.operation.NullableSchemaInvariant
import io.taig.otter.operation.RecordSchemaInvariant
import io.taig.otter.operation.SchemaInvariant

trait ComparisonComponent[Nullable[a] <: Value[a], Record[a] <: Value[a], Field[_], Key[_], Value[_]](using
    FieldSchemaInvariant[Field, Key, Value],
    NullableSchemaInvariant[Nullable, Value],
    RecordSchemaInvariant[Record, Field],
    SchemaInvariant.Nullable[Value, Nullable]
) extends FieldComponent.Primitive.String[Field, Key, Value, Record],
      NullableComponent[Nullable, Value],
      PrimitiveComponent.Boolean[Value],
      RecordComponent[Record, Field]:
  def comparison[A](schema: => Value[A]): Record[Comparison[A]] =
    (field("reference", schema) :* field("exclusive", boolean.nullable(default = false))).to
