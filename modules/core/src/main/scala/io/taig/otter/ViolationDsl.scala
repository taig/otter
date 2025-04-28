package io.taig.otter

trait ViolationDsl[
    +Collection[a] <: Value[a],
    +Dictionary[a] <: Value[a],
    +Nullable[a] <: Value[a],
    +Primitive[a] <: Value[a],
    +Record[a] <: Value[a],
    +Union[a] <: Value[a],
    Key[_],
    Value[_]
](using Codec.Record[Record, Key, Value])
    extends DataDsl[Collection, Dictionary, Nullable, Primitive, Union, Key, Value],
      ConstraintDsl[Collection, Dictionary, Nullable, Primitive, Record, Union, Key, Value],
      NullableDsl[Nullable, Value],
      PrimitiveDsl[Primitive],
      RecordDsl.Primitive.String[Record, Key, Value]:

  val violation: Record[Violation] = (
    field("constraint", constraint) :*
      field("actual", data.any) :*
      field("hint", nullable(string))
  ).to
