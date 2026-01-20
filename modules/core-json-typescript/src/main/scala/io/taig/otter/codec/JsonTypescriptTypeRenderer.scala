package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Typescript

object JsonTypescriptTypeRenderer extends Renderer[Json.Write, Typescript.Type]:
  override def render[A](json: Json.Write[A]): Typescript.Type = json match
    case json: Json.Constant.Write[A]    => json.encode(JsonPrimitiveTypescriptTypeEncoder)
    case json: Json.Collection.Write[A]  => ???
    case json: Json.Dictionary.Write[A]  => ???
    case json: Json.Enumeration.Write[A] =>
      Typescript.Type.Union(
        types = json.encode(JsonPrimitiveTypescriptTypeEncoder).toNonEmptyList
      )
    case json: Json.Optional.Write[A] => ???
    case json: Json.Primitive.Boolean.Write[A] => Typescript.Type.Symbol(name = "boolean", parameters = Nil)
    case json: Json.Primitive.Coerce.Write[A] => ???
    case json: Json.Primitive.Number.Write[A] => Typescript.Type.Symbol(name = "number", parameters = Nil)
    case json: Json.Primitive.Text.Write[A] => Typescript.Type.Symbol(name = "string", parameters = Nil)
    case json: Json.Record.Write[A] => ???
    case json: Json.Tuple.Write[A] => ???
    case json: Json.Union.Write[A] => ???


