package io.taig.otter.component

import cats.Invariant
import io.taig.otter.Violation
import io.taig.otter.syntax.InvariantSyntax.*

trait ViolationComponent[
    Collection[a] <: Value[a],
    Dictionary[a] <: Value[a],
    Nullable[a] <: Value[a],
    Primitive[a] <: Value[a],
    Record[a] <: Value[a]: Invariant,
    Sum[a] <: Value[a],
    Union[a] <: Value[a],
    Branch[_],
    Field[_],
    Key[_],
    Value[_]
] extends DataComponent[Collection, Dictionary, Nullable, Primitive, Sum, Branch, Key, Value],
      ConstraintComponent[Collection, Dictionary, Nullable, Primitive, Record, Sum, Branch, Field, Key, Value],
      NullableComponent[Nullable, Value],
      PrimitiveComponent[Primitive],
      FieldComponent.Primitive.String[Field, Key, Value, Record]:

  val violation: Record[Violation] = (
    field("constraint", constraint) :*
      field("actual", data.any) :*
      field("hint", nullable(string))
  ).to
