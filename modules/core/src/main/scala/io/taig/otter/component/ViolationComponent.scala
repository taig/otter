package io.taig.otter.component

import io.taig.otter.operation.*
import io.taig.otter.syntax.EnrichedSyntax.*
import io.taig.otter.validation.Constraint
import io.taig.otter.validation.Violation

trait ViolationComponent[
    Collection[a] <: Value[a],
    Constant[a] <: Value[a],
    Dictionary[a] <: Value[a],
    Primitive[a] <: Value[a],
    Record[a] <: Value[a],
    Union[a] <: Value[a],
    Field[_],
    Key[_],
    Value[_]
](using
    SchemaInvariant.Nullable[Value, Value],
    SchemaInvariant.Recordable[Field, Record],
    RecordSchemaInvariant[Record, Field]
) extends ConstraintComponent[Collection, Constant, Dictionary, Primitive, Record, Union, Field, Key, Value],
      DataComponent[Collection, Dictionary, Primitive, Union, Key, Value],
      FieldComponent[Field, Key, Value]:
  this: PrimitiveComponent[Value] =>

  val violation: Record[Violation[?]] = (
    field("constraint", constraint) :*
      field("actual", data.any) :*
      field("hint", string.nullable)
  ).name("Violation").to
