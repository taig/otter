package io.taig.otter

trait ComparisonDsl[Record[_], Key[_], Value[_]](using Codec.Record[Record, Key, Value])
    extends RecordDsl.Primitive.String[Record, Key, Value],
      PrimitiveDsl.Boolean[Value]:
  def comparison[A](codec: => Value[A]): Record[Comparison[A]] =
    (field("reference", codec) :* field("exclusive", boolean)).to
