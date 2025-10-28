package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.syntax.JsonSyntax.*

object JsonConstantZodRenderer extends Renderer[Json.Constant, String]:
  val printer = PrimitivePrinter.contramapK[Json.Primitive]([A] => (self: Json.Primitive[A]) => self.self.self)

  override def render[A](json: Json.Constant[A]): String = json.schema.value match
    case Json.Primitive.Boolean(self) => s"z.literal(${json.self.self.encode(printer)})"
    case Json.Primitive.Number(self)  => s"z.literal(${json.self.self.encode(printer)})"
    case Json.Primitive.String(self)  => s"z.literal(\"${json.self.self.encode(printer).replace("\"", "\\\"")}\")"
