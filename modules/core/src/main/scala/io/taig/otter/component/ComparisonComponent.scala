package io.taig.otter.component

import cats.Invariant
import io.taig.otter.Comparison
import io.taig.otter.syntax.InvariantSyntax.*
import io.taig.otter.schema.RecordSchema
import io.taig.otter.schema.NullableSchema

trait ComparisonComponent[Nullable[a] <: Value[a], Record[_], Field[_], Key[_], Value[_]](using
    RecordSchema[Record, Field],
    NullableSchema[Nullable, Value]
) extends FieldComponent.Primitive.String[Field, Key, Value],
      NullableComponent[Nullable, Value],
      PrimitiveComponent.Boolean[Value],
      RecordComponent[Record, Field]:
  def comparison[A](schema: => Value[A]): Record[Comparison[A]] =
    (field("reference", schema) :* field("exclusive", boolean.nullable(default = false))).to
