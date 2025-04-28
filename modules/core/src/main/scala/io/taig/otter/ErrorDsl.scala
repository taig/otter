package io.taig.otter

trait ErrorDsl[+Constant[a] <: Value[a], +Record[_], -Key[_], -Value[_]](using Codec.Record[Record, Key, Value])
    extends ConstantDsl.Primitive.String[Constant, Value]:
  this: PrimitiveDsl.String[Value] & RecordDsl.Primitive.String[Record, Key, Value] =>

  def error[A](tpe: String, codec: => Value[A]): Record[A] =
    field(name = "error", codec = constant(tpe)) :* field(name = "value", codec)

  def error[A](tpe: String): Record[Unit] = ??? // field(name = "error", codec = constant(tpe))
