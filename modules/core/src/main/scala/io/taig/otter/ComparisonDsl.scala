package io.taig.otter

trait ComparisonDsl[Record[_], Field[_], Key[_], Value[_]](using
    Codec.Field[Field, Key, Value, Record],
    Codec.Record[Record, Field]
) extends PrimitiveDsl.Boolean[Value],
      FieldDsl.Primitive.String[Field, Key, Value, Record]:
  def comparison[A](codec: => Value[A]): Record[Comparison[A]] =
    (field("reference", codec) :* field("exclusive", boolean)).to
