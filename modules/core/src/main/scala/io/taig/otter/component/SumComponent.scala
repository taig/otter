package io.taig.otter.component

import io.taig.otter.operation.RecordSchemaInvariant
import io.taig.otter.operation.SchemaInvariant

trait SumComponent[
    Constant[a] <: Value[a],
    Record[a] <: Value[a],
    Field[_],
    Key[_],
    Value[_]
](using
    RecordSchemaInvariant[Record, Field],
    SchemaInvariant.Recordable[Field, Record]
) extends ConstantComponent.Primitive.String[Constant, Value],
      FieldComponent.Primitive.String[Field, Key, Value]:
  this: PrimitiveComponent[Value] =>

  def explicit(name: String): Record[Unit] = field(name = "type", constant(name)).toRecord

  def explicit[A](name: String, schema: => Value[A]): Record[A] =
    explicit(name) :* field(name = "value", schema)

  def merged(name: String): Record[Unit] = field(name = "type", constant(name)).toRecord

  def merged[A](name: String, schema: => Record[A]): Record[A] =
    merged(name).zip(schema).merged

  def keyed[A](name: String, schema: => Value[A]): Record[A] = field(name, schema).toRecord
