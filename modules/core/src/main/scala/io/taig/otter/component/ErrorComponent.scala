package io.taig.otter.component

import io.taig.otter.schema.RecordSchema

trait ErrorComponent[Constant[a] <: Value[a], Record[a] <: Value[a], Field[_], Key[_], Value[_]](using
    RecordSchema[Record, Field]
) extends ConstantComponent.Primitive.String[Constant, Value],
      FieldComponent.Primitive.String[Field, Key, Value],
      RecordComponent[Record, Field]:
  this: PrimitiveComponent.String[Value] =>

  def error[A](tpe: String, schema: => Value[A]): Record[A] =
    field(name = "error", schema = constant(tpe)) :* field(name = "value", schema)

  def error[A](tpe: String): Record[Unit] = field(name = "error", schema = constant(tpe)).toRecord
