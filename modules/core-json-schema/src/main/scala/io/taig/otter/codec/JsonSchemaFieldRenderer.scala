package io.taig.otter.codec

import cats.data.Chain
import io.circe.Json as CirceJson
import io.taig.otter.Json

final class JsonSchemaFieldRenderer(renderer: Renderer[Json, CirceJson])
    extends Renderer[Json.Field, Chain[(String, CirceJson)]]:
  val self = FieldRenderer(renderer).contramapK[Json.Field]([A] => (json: Json.Field[A]) => json.self.self)

  override def render[A](schema: Json.Field[A]): Chain[(String, CirceJson)] = self.render(schema)

object JsonSchemaFieldRenderer:
  def apply(renderer: Renderer[Json, CirceJson]): Renderer[Json.Field, Chain[(String, CirceJson)]] =
    new JsonSchemaFieldRenderer(renderer)
