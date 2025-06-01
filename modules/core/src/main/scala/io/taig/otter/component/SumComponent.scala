package io.taig.otter.component
import io.taig.otter.operation.FieldSchemaInvariant
import io.taig.otter.operation.RecordSchemaInvariant

trait SumComponent[
    Constant[a] <: Value[a],
    Primitive[a] <: Value[a],
    Record[a] <: Value[a],
    Field[_],
    Key[_],
    Value[_]
](using
    FieldSchemaInvariant[Field, Key, Value],
    RecordSchemaInvariant[Record, Field]
) extends ConstantComponent.Primitive.String[Constant, Primitive, Value],
      FieldComponent.Primitive.String[Field, Key, Value, Record],
      RecordComponent[Record, Field]:
  this: PrimitiveComponent.String[Primitive, Primitive] =>

  def explicit(name: String): Record[Unit] = field(name = "type", constant(name)).toRecord

  def explicit[A](name: String, schema: => Value[A]): Record[A] =
    explicit(name) :* field(name = "value", schema)

  def merged(name: String): Record[Unit] = field(name = "type", constant(name)).toRecord

  def merged[A](name: String, schema: => Record[A]): Record[A] =
    merged(name).zip(schema).merge

  def keyed[A](name: String, schema: => Value[A]): Record[A] = field(name, schema).toRecord
