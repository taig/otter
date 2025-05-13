package io.taig.otter

trait ViolationDsl[
    Collection[a] <: Value[a],
    Dictionary[a] <: Value[a],
    Nullable[a] <: Value[a],
    Primitive[a] <: Value[a],
    Record[a] <: Value[a],
    Union[a] <: Value[a],
    Field[_],
    Key[_],
    Value[_]
](using Codec.Record[Record, Field], Codec.Field[Field, Key, Value, Record])
    extends DataDsl[Collection, Dictionary, Nullable, Primitive, Union, Key, Value],
      ConstraintDsl[Collection, Dictionary, Nullable, Primitive, Record, Union, Field, Key, Value],
      NullableDsl[Nullable, Value],
      PrimitiveDsl[Primitive],
      FieldDsl.Primitive.String[Field, Key, Value, Record]:

  val violation: Record[Violation] = (
    field("constraint", constraint) :*
      field("actual", data.any) :*
      field("hint", nullable(string))
  ).to
