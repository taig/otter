package io.taig.otter.component

import io.taig.otter.Violation
import io.taig.otter.operation.*
import io.taig.otter.syntax.SchemaInvariantSyntax.*
import io.taig.otter.syntax.EnrichedSyntax.*

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
](using
    FieldSchemaInvariant[Field, Key, Value],
    NullableSchemaInvariant[Nullable, Value],
    RecordSchemaInvariant[Record, Field]
) extends DataComponent[Collection, Constant, Dictionary, Nullable, Primitive, Record, Union, Field, Key, Value],
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
  ).name("Violation").to
