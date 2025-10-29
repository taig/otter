package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Primitive

val JsonPrimitiveZodPrinter: Printer[Json.Primitive] = PrimitivePrinter
  .contramapK[Json.Primitive]([A] => (json: Json.Primitive[A]) => json.annotation.self)
  .mapWithSchema: [A] =>
    (schema: Json.Primitive[A], value: String) =>
      schema match
        case _: Json.Primitive.String[?] => s"\"${value.replace("\"", "\\\"")}\""
        case _                           => value
