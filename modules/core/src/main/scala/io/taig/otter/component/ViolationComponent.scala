package io.taig.otter.component

import io.taig.otter.Keys.name
import io.taig.otter.Violation
import io.taig.otter.schema.*

trait ViolationComponent[
    Collection[a] <: Value[a],
    Constant[a] <: Value[a],
    Dictionary[a] <: Value[a],
    Nullable[a] <: Value[a],
    Primitive[a] <: Value[a],
    Record[a] <: Value[a],
    Union[a] <: Value[a],
    Field[_],
    Key[_],
    Value[_]
](using FieldSchema[Field, Key, Value], NullableSchema[Nullable, Value], EnrichedRecordSchema[Record, Field])
    extends DataComponent[Collection, Constant, Dictionary, Nullable, Primitive, Record, Union, Field, Key, Value],
      ConstraintComponent[Collection, Constant, Dictionary, Nullable, Primitive, Record, Union, Field, Key, Value],
      NullableComponent[Nullable, Value],
      PrimitiveComponent[Primitive],
      FieldComponent.Primitive.String[Field, Key, Value, Record],
      RecordComponent[Record, Field]:
  this: PrimitiveComponent.String[Value] =>

  val violation: Record[Violation] = (
    field("constraint", constraint) :*
      field("actual", data.any) :*
      field("hint", string.nullable)
  ).metadata(name, "Violation").to
