package io.taig.otter.component
import io.taig.otter.schema.FieldSchema
import io.taig.otter.schema.RecordSchema

trait SumComponent[Constant[a] <: Value[a], Record[a] <: Value[a], Field[_], Key[_], Value[_]](using FieldSchema[Field, Key, Value, Record], RecordSchema[Record, Field])
    extends ConstantComponent.Primitive.String[Constant, Value],
      FieldComponent.Primitive.String[Field, Key, Value, Record],
      RecordComponent[Record, Field]:
  this: PrimitiveComponent.String[Value] =>

  def explicit(name: String): Record[Unit] = field(name = "type", constant(name)).toRecord

  def explicit[A](name: String, schema: => Value[A]): Record[A] =
    explicit(name) :* field(name = "value", schema)

  def merged(name: String): Record[Unit] = field(name = "type", constant(name)).toRecord

  def merged[A](name: String, schema: => Record[A]): Record[A] =
    merged(name).zip(schema).merge

  def keyed[A](name: String, schema: => Value[A]): Record[A] = field(name, schema).toRecord
