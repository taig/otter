package io.taig.otter

trait ConstraintDsl[Record[a] <: Value[a], Union[_], Key[_], Value[_]](using
    Codec.Record[Record, Key, Value],
    Codec.Union[Union, Value]
) extends PrimitiveDsl[Value],
      RecordDsl.Primitive.String[Record, Key, Value],
      UnionDsl[Union, Value]:
  val constraint: Union[Constraint] =
    branch("type", field("name", string)).to[Constraint.Type]
    ???
