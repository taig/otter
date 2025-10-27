package io.taig.otter.codec

import cats.syntax.all.*
import io.circe.JsonObject as CirceJsonObject
import io.circe.Json as CirceJson
import io.circe.syntax.*
import io.taig.otter.Field
import io.taig.otter.Json
import io.taig.otter.JsonSchemaExpression
import cats.Functor

final class JsonSchemaFieldRenderer[F[_]: Functor](
    renderer: Renderer[Json, F[JsonSchemaExpression]],
    encoder: Encoder[Json, CirceJson]
) extends Renderer[Json.Field, F[(String, JsonSchemaExpression)]]:
  val self = FieldRenderer(renderer).contramapK[Json.Field]([A] => (json: Json.Field[A]) => json.self.self)

  override def render[A](json: Json.Field[A]): F[(String, JsonSchemaExpression)] =
    val properties = CirceJsonObject("default" := encode(field = json.self.self, none))
      .filter((_, value) => value != CirceJson.Null)

    self
      .render(json)
      .map:
        _.fmap:
          case JsonSchemaExpression.Inline(json) =>
            JsonSchemaExpression.Inline(json.deepMerge(properties.toJson))
          case JsonSchemaExpression.Reference(name, data) =>
            JsonSchemaExpression.Reference(name, data.deepMerge(properties))

  def encode[A](field: Field[Json, A], a: Option[A]): Option[CirceJson] = field match
    case Field.Default(self, default) => encode(field = self, default.value.some)
    case Field.Modify(self, _, g)     => encode(field = self, a.map(g))
    case Field.Optional(self)         => encode(field = self, none)
    case Field.Root(_, schema)        => a.map(encoder.encode(schema = schema.value, _))

object JsonSchemaFieldRenderer:
  def apply[F[_]: Functor](
      renderer: Renderer[Json, F[JsonSchemaExpression]],
      encoder: Encoder[Json, CirceJson]
  ): Renderer[Json.Field, F[(String, JsonSchemaExpression)]] = new JsonSchemaFieldRenderer(renderer, encoder)
