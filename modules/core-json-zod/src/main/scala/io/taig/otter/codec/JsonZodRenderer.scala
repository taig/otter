package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.syntax.JsonSyntax.*
import scala.util.chaining.*

object JsonZodRenderer extends Renderer[Json, String]:
  override def render[A](json: Json[A]): String = json match
    case json @ Json.Coerce(_)               => JsonCoerceZodRenderer.render(json)
    case json @ Json.Constant(_)             => s"z.literal(${json.encode(JsonPrimitiveZodPrinter)})"
    case Json.Collection(annotation)         => s"z.array(${render(json = annotation.self.schema.value)})"
    case Json.Dictionary(annotation)         => s"z.record(z.string(), ${render(json = annotation.self.schema.value)})"
    case json @ Json.Enumeration(annotation) =>
      s"z.enum(${json.encode(JsonPrimitiveZodPrinter).mkString_("[", ", ", "]")})"
    case Json.Primitive.Boolean(_) => "z.boolean()"
    case Json.Primitive.Number(_)  => "z.number()"
    case Json.Primitive.String(_)  => "z.string()"
    case json @ Json.Nullable(_)   => s"z.nullable(${render(json = json.schema.value)})"
    case json @ Json.Record(_)     =>
      val fields = json.fields
        .map(_.value)
        .map: field =>
          val value = render(json = field.schema.value).pipe: value =>
            if field.isOptional then s"z.optional($value)" else value

          s""""${field.name}": $value"""

      s"z.object(${fields.mkString_("{\n", ",\n", "\n}")})"
    case json @ Json.Tuple(_) => s"z.tuple(${json.schemas.map(_.value).map(render).mkString_("[", ", ", "]")})"
    case json @ Json.Union(_) => json.schemas.map(_.value).map(render).mkString_("z.union([", ", ", "])")
