package io.taig.otter.codec

import cats.Functor
import cats.syntax.all.*
import io.circe.Encoder as CirceEncoder
import io.circe.Json as CirceJson
import io.circe.JsonObject as CirceJsonObject
import io.circe.syntax.*
import io.taig.otter.Field
import io.taig.otter.Json
import io.taig.otter.JsonSchemaExpression

final class JsonSchemaFieldRenderer[F[_]: Functor](
    encoder: Encoder[Json, CirceJson],
    renderer: Renderer[Json, F[JsonSchemaExpression]]
) extends Renderer[Json.Field, F[(String, JsonSchemaExpression)]]:
  val self: Renderer[Json.Field, F[(String, JsonSchemaExpression)]] = FieldRenderer(renderer)
    .contramapK([A] => (json: Json.Field[A]) => json.annotation.self)

  override def render[A](json: Json.Field[A]): F[(String, JsonSchemaExpression)] =
    val properties = CirceJsonObject("default" := encode(field = json.annotation.self, none))
      .filter((_, value) => value != CirceJson.Null)

    self.render(json).map(_.map(_.merge(properties)))

  def encode[A](field: Field[Json, A], a: Option[A]): Option[CirceJson] = field match
    case Field.Default(self, default) => encode(field = self, default.value.some)
    case Field.Modify(self, _, g)     => encode(field = self, a.map(g))
    case Field.Optional(self)         => encode(field = self, none)
    case Field.Root(_, schema)        => a.map(encoder.encode(schema = schema.value, _))

object JsonSchemaFieldRenderer:
  def apply[F[_]: Functor](
      encoder: Encoder[Json, CirceJson],
      renderer: Renderer[Json, F[JsonSchemaExpression]]
  ): Renderer[Json.Field, F[(String, JsonSchemaExpression)]] = new JsonSchemaFieldRenderer(encoder, renderer)
