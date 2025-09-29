package io.taig.otter.component

import io.taig.otter.operation.SchemaInvariant
import io.taig.otter.validation.Comparison

trait ComparisonComponent[Record[a] <: Value[a], Field[_], Key[_], Value[_]](using
    SchemaInvariant[Record],
    SchemaInvariant.Nullable[Value, Value],
    SchemaInvariant.Recordable[Field, Record]
) extends FieldComponent.Primitive.String[Field, Key, Value],
      PrimitiveComponent.Boolean[Value]:
  def comparison[A](schema: => Value[A]): Record[Comparison[A]] =
    (field("reference", schema) :* field("exclusive", boolean.nullable(default = false))).to
