package io.taig.otter.codec

import cats.Applicative
import cats.syntax.all.*
import io.circe.Encoder as CirceEncoder
import io.circe.Json as CirceJson
import io.circe.syntax.*
import io.taig.otter.Json
import io.taig.otter.JsonSchemaExpression
import io.taig.otter.JsonSchemaKeys
import io.taig.otter.Keys
import io.taig.otter.Metadata
import io.taig.otter.Primitive

final case class JsonSchemaInlineRenderer[F[_]: Applicative](
    encoder: Encoder[Json, CirceJson],
    renderer: Renderer[Json, F[JsonSchemaExpression]]
) extends Renderer[Json, F[CirceJson]]:
  val record = RecordRenderer(renderer = JsonSchemaFieldRenderer(encoder, renderer))

  override def render[A](json: Json[A]): F[CirceJson] = ???
  // json match
  //   case Json.Coerce(annotation) =>
  //     render(metadata = annotation.metadata).deepMerge(CirceJson.obj("type" := "string")).pure
  //   case Json.Collection(annotation) =>
  //     renderer
  //       .render(schema = annotation.self.schema.value)
  //       .map: items =>
  //         render(metadata = annotation.metadata).deepMerge(
  //           CirceJson.obj("type" := "array", "items" := items)
  //         )

  //   case json @ Json.Constant(annotation) =>
  //     render(metadata = annotation.metadata).deepMerge(CirceJson.obj("const" := json.encode(encoder))).pure
  //   case Json.Dictionary(annotation) =>
  //     renderer
  //       .render(schema = annotation.self.schema.value)
  //       .map: additionalProperties =>
  //         render(metadata = annotation.metadata).deepMerge(
  //           CirceJson.obj("type" := "object", "additionalProperties" := additionalProperties)
  //         )
  //   case json @ Json.Enumeration(annotation) =>
  //     render(json = annotation.self.schema.value).map: data =>
  //       render(metadata = annotation.metadata)
  //         .deepMerge(data.asJson)
  //         .deepMerge(CirceJson.obj("enum" := json.encode(encoder)))
  //   case json @ Json.Nullable(annotation) =>
  //     render(json = annotation.self.schema.value).map: data =>
  //       render(metadata = annotation.metadata)
  //         .deepMerge(data)
  //         .deepMerge(CirceJson.obj("default" := json.encode(encoder)).dropNullValues)
  //   case Json.Primitive.Boolean(annotation) =>
  //     render(metadata = annotation.metadata).deepMerge(CirceJson.obj("type" := "boolean")).pure
  //   case Json.Primitive.Number(annotation) =>
  //     render(metadata = annotation.metadata).deepMerge(CirceJson.obj("type" := tpe(schema = annotation.self))).pure
  //   case Json.Primitive.String(annotation) =>
  //     render(metadata = annotation.metadata).deepMerge(CirceJson.obj("type" := "string")).pure
  //   case json @ Json.Record(annotation) =>
  //     record
  //       .render(annotation.self)
  //       .map: properties =>
  //         render(metadata = annotation.metadata).deepMerge(
  //           CirceJson
  //             .obj(
  //               "properties" := CirceJson.fromFields(properties.map((key, value) => key := value).toIterable),
  //               "required" := json.fields
  //                 .map(_.value)
  //                 .mapFilter(field => Option.when(!field.isOptional)(field.name)),
  //               "type" := "object"
  //             )
  //         )
  //   case Json.Tuple(annotation) =>
  //     annotation.self.schemas
  //       .map(_.value)
  //       .traverse(render)
  //       .map: expressions =>
  //         render(metadata = annotation.metadata).deepMerge(
  //           CirceJson.obj(
  //             "type" := "array",
  //             "minItems" := annotation.self.size,
  //             "prefixItems" := expressions,
  //             "items" := false
  //           )
  //         )
  //   case Json.Union(annotation) =>
  //     annotation.self.branches
  //       .map(_.value)
  //       .traverse(render)
  //       .map: expressions =>
  //         render(metadata = annotation.metadata)
  //           .deepMerge(CirceJson.obj("oneOf" := expressions))

  def render(metadata: Metadata): CirceJson = CirceJson
    .obj(
      "description" := metadata.get(JsonSchemaKeys.description).orElse(metadata.get(Keys.description)),
      "title" := metadata.get(JsonSchemaKeys.title).orElse(metadata.get(Keys.title))
    )
    .dropNullValues

  def tpe(schema: Primitive.Number[?]): String = schema match
    case Primitive.Number.BigDecimal(_)      => "number"
    case Primitive.Number.BigInteger(_)      => "integer"
    case Primitive.Number.Double(_)          => "number"
    case Primitive.Number.Float(_)           => "number"
    case Primitive.Number.Int(_)             => "integer"
    case Primitive.Number.Long(_)            => "integer"
    case Primitive.Number.Modify(self, _, _) => tpe(schema = self)
