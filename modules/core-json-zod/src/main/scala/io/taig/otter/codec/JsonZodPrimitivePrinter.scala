package io.taig.otter.codec

import io.taig.otter.Json

object JsonZodPrimitivePrinter extends Printer[Json.Primitive.Write]:
  val printer = PrimitivePrinter(printer = this)
    .contramapK[Json.Primitive.Write]([A] => (json: Json.Primitive.Write[A]) => json.self.self)
    .mapWith: [A] =>
      (json: Json.Primitive.Write[A], value: String) =>
        json match
          case _: Json.Primitive.Text.Write[?] => s"\"${value.replace("\"", "\\\"")}\""
          case _                               => value

  override def encode[A](json: Json.Primitive.Write[A], a: A): String = printer.encode(json, a)
