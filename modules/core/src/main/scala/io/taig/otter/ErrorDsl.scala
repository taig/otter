// package io.taig.otter

// trait ErrorDsl[+Constant[a] <: Value[a], +Record[_], Field[_], -Key[_], -Value[_]](using
//     Codec.Field[Field, Key, Value, Record]
// ) extends ConstantDsl.Primitive.String[Constant, Value]:
//   this: PrimitiveDsl.String[Value] & FieldDsl.Primitive.String[Field, Key, Value, Record] =>

//   def error[A](tpe: String, codec: => Value[A]): Record[A] =
//     field(name = "error", codec = constant(tpe)) :* field(name = "value", codec)

//   def error[A](tpe: String): Record[Unit] = field(name = "error", codec = constant(tpe)).toRecord
