// package io.taig.otter

// object JsonPrimitivePrinter extends Printer[Json.Primitive]:
//   override def apply[A](codec: Json.Primitive[A], a: A): String =
//     PrimitivePrinter.Quoted(codec = codec.self, a)
