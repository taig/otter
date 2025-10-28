package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.syntax.JsonSyntax.*

final class JsonZodRenderer extends Renderer[Json, String]:
  override def render[A](json: Json[A]): String = json match
    case json @ Json.Coerce(_)       => JsonCoerceZodRenderer.render(json)
    case json @ Json.Constant(_)     => JsonConstantZodRenderer.render(json)
    case Json.Collection(annotation) => s"z.array(${render(json = annotation.self.schema.value)})"
    case Json.Primitive.Boolean(_)   => "z.boolean()"
    case Json.Primitive.Number(_)    => "z.number()"
    case Json.Primitive.String(_)    => "z.string()"
    case Json.Nullable(annotation)   => s"z.nullable(${render(json = annotation.self.schema.value)})"
