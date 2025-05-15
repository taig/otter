package io.taig.otter.component

import io.taig.otter.Schema

// trait Component[Nullable[a] <: Value[a], Record[_], Field[_], Key[_], Value[_]](using
//       Schema.Field[Field, Key, Value],
//       Schema.Record[Record, Field]
//   ) extends Nullable.Component[Nullable, Value],
//         Primitive.Component.Boolean[Value],
//         Field.Component.Primitive.String[Field, Key, Value, Record]:
//     def comparison[A](codec: => Value[A]): Record[Comparison[A]] = ???
//     // (field("reference", codec) :* field("exclusive", nullable(boolean, default = false))).to
