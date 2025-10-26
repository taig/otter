package io.taig.otter.codec

import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.circe.syntax.*
import io.taig.otter.Json
import io.taig.otter.syntax.JsonSyntax.*
import io.taig.otter.Keys
import io.taig.otter.JsonSchemaKeys
import io.taig.otter.Keys.*
import io.taig.otter.Enumeration
import io.taig.otter.Primitive
import io.taig.otter.Metadata
import io.taig.otter.Enumeration.Modify
import io.taig.otter.Enumeration.Root
import cats.data.NonEmptyList
import cats.data.Chain

final class JsonSchemaRenderer(encoder: Encoder[Json, CirceJson]) extends Renderer[Json, CirceJson]:
  val record = RecordRenderer(renderer = JsonSchemaFieldRenderer(renderer = this, encoder))
  override def render[A](json: Json[A]): CirceJson = json match
    case Json.Coerce(annotation) =>
      render(metadata = annotation.metadata).deepMerge(CirceJson.obj("type" := "string"))
    case Json.Collection(annotation) =>
      render(metadata = annotation.metadata).deepMerge(
        CirceJson.obj(
          "type" := "array",
          "items" := render(json = annotation.self.schema.value)
        )
      )
    case json @ Json.Constant(annotation) =>
      render(metadata = annotation.metadata)
        .deepMerge(CirceJson.obj("const" := json.encode(encoder)))
    case Json.Enumeration(annotation) =>
      render(metadata = annotation.metadata)
        .deepMerge(render(json = annotation.self.schema.value))
        .deepMerge(
          CirceJson.obj("enum" := render(schema = annotation.self))
        )
    case Json.Primitive.Boolean(annotation) =>
      render(metadata = annotation.metadata).deepMerge(CirceJson.obj("type" := "boolean"))
    case Json.Primitive.Number(annotation) =>
      render(metadata = annotation.metadata).deepMerge(render(schema = annotation.self))
    case Json.Primitive.String(annotation) =>
      render(metadata = annotation.metadata).deepMerge(CirceJson.obj("type" := "string"))
    case json @ Json.Record(annotation) =>
      render(metadata = annotation.metadata).deepMerge(
        CirceJson
          .obj(
            "properties" := record.render(annotation.self),
            "required" := json.fields
              .map(_.value)
              .mapFilter(field => Option.when(!field.isOptional)(field.name)),
            "type" := "object"
          )
      )

  def render[A](schema: Enumeration[Json.Primitive, A]): NonEmptyList[CirceJson] = schema match
    case Enumeration.Modify(self, _, _)    => render(schema = self)
    case Enumeration.Root(schema, mapping) =>
      mapping.values
        .map(mapping.inj)
        .map(encoder.encode(schema = schema.value, _))

  def render[A](schema: Primitive.Number[A]): CirceJson = schema match
    case Primitive.Number.BigDecimal(_)      => CirceJson.obj("type" := "number")
    case Primitive.Number.BigInteger(_)      => CirceJson.obj("type" := "integer")
    case Primitive.Number.Double(_)          => CirceJson.obj("type" := "number")
    case Primitive.Number.Float(_)           => CirceJson.obj("type" := "number")
    case Primitive.Number.Int(_)             => CirceJson.obj("type" := "integer")
    case Primitive.Number.Long(_)            => CirceJson.obj("type" := "integer")
    case Primitive.Number.Modify(self, _, _) => render(schema = self)

  def render(metadata: Metadata): CirceJson = CirceJson
    .obj(
      "description" := metadata.get(JsonSchemaKeys.description).orElse(metadata.get(Keys.description)),
      "title" := metadata.get(JsonSchemaKeys.title).orElse(metadata.get(Keys.title))
    )
    .dropNullValues
