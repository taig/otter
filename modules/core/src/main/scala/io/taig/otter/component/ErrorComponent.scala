package io.taig.otter.component

import io.taig.otter.Keys.name
import io.taig.otter.operation.*
import io.taig.otter.syntax.EnrichedSyntax.*

trait ErrorComponent[Constant[a] <: Value[a], Primitive[a] <: Value[a], Record[a] <: Value[a], Field[_], Key[_], -Value[
    _
]](using
    SchemaInvariant.Recordable[Field, Record],
    RecordSchemaInvariant[Record, Field]
) extends ConstantComponent.Primitive.String[Constant, Primitive],
      FieldComponent.Primitive.String[Field, Key, Value]:
  this: PrimitiveComponent[Primitive] =>

  def error[A](tpe: String, schema: => Value[A]): Record[A] = (
    field(name = "error", schema = constant(tpe)) :*
      field(name = "value", schema)
  ).metadata(name, tpe.capitalize)

  def error[A](tpe: String): Record[Unit] = field(name = "error", schema = constant(tpe)).toRecord
    .metadata(name, tpe.capitalize)
