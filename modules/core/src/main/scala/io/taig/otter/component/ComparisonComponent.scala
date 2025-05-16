package io.taig.otter.component

import io.taig.otter.Comparison
import io.taig.otter.syntax.InvariantSyntax.*
import cats.Invariant

trait ComparisonComponent[Nullable[a] <: Value[a], Record[_]: Invariant, Field[_], Key[_], Value[_]]
    extends FieldComponent.Primitive.String[Field, Key, Value, Record],
      NullableComponent[Nullable, Value],
      PrimitiveComponent.Boolean[Value],
      RecordComponent[Record, Field]:
  def comparison[A](schema: => Value[A]): Record[Comparison[A]] =
    (field("reference", schema) :* field("exclusive", boolean.nullable(default = false))).to
