package io.taig.otter

trait ComparisonDsl[+Record[_], -Key[_], -Value[_]](using Codec.Record[Record, Key, Value]):
  this: PrimitiveDsl.Boolean[Value] & RecordDsl.Primitive.String[Record, Key, Value] =>
  def comparison[A](codec: => Value[A]): Record[Comparison[A]] =
    (field("reference", codec) :* field("exclusive", boolean)).to
