package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Typescript

object JsonZodPrimitivePrinter extends Encoder[Json.Primitive.Write, Typescript.Expression]:
  val printer = TypescriptExpressionPrimitivePrinter(printer = this)
    .contramapK[Json.Primitive.Write]([A] => (json: Json.Primitive.Write[A]) => json.self.self)

  override def encode[A](json: Json.Primitive.Write[A], a: A): Typescript.Expression = printer.encode(json, a)
