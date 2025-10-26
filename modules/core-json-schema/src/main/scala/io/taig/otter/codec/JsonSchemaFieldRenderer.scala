package io.taig.otter.codec

import cats.data.Chain
import cats.syntax.all.*
import io.circe.syntax.*
import io.circe.Json as CirceJson
import io.taig.otter.Json
import io.taig.otter.syntax.JsonSyntax.*
import io.taig.otter.Field

final class JsonSchemaFieldRenderer(renderer: Renderer[Json, CirceJson], encoder: Encoder[Json, CirceJson])
    extends Renderer[Json.Field, (String, CirceJson)]:
  val self = FieldRenderer(renderer).contramapK[Json.Field]([A] => (json: Json.Field[A]) => json.self.self)

  override def render[A](schema: Json.Field[A]): (String, CirceJson) =
    val properties = CirceJson
      .obj("default" := encode(field = schema.self.self, none))
      .dropNullValues

    self.render(schema).map(_.deepMerge(properties))

  def encode[A](field: Field[Json, A], a: Option[A]): Option[CirceJson] = field match
    case Field.Default(self, default) => encode(field = self, default.value.some)
    case Field.Modify(self, _, g)     => encode(field = self, a.map(g))
    case Field.Optional(self)         => encode(field = self, none)
    case Field.Root(_, schema)        => a.map(encoder.encode(schema = schema.value, _))

object JsonSchemaFieldRenderer:
  def apply(
      renderer: Renderer[Json, CirceJson],
      encoder: Encoder[Json, CirceJson]
  ): Renderer[Json.Field, (String, CirceJson)] =
    new JsonSchemaFieldRenderer(renderer, encoder)
