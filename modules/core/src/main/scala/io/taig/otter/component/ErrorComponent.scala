package io.taig.otter.component

import io.taig.otter.schema.FieldSchema
import io.taig.otter.Keys.name
import io.taig.otter.schema.EnrichedRecordSchema

trait ErrorComponent[Constant[a] <: Value[a], Record[a] <: Value[a], Field[_], Key[_], Value[_]](using
    FieldSchema[Field, Key, Value],
    EnrichedRecordSchema[Record, Field]
) extends ConstantComponent.Primitive.String[Constant, Value],
      FieldComponent.Primitive.String[Field, Key, Value, Record],
      RecordComponent[Record, Field]:
  this: PrimitiveComponent.String[Value] =>

  def error[A](tpe: String, schema: => Value[A]): Record[A] =
    (field(name = "error", schema = constant(tpe)) :* field(name = "value", schema))
      .metadata(name, tpe.capitalize)

  def error[A](tpe: String): Record[Unit] = field(name = "error", schema = constant(tpe)).toRecord
    .metadata(name, tpe.capitalize)
