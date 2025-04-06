package io.taig.otter

object JsonKeyPrimitivePrinter extends Printer[Json.Key.Primitive]:
  override def apply[A](codec: Json.Key.Primitive[A], a: A): String =
    PrimitivePrinter.Quoted(codec = codec.self, a)
