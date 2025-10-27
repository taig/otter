package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.circe.syntax.*
import cats.data.State
import cats.syntax.all.*
import scala.collection.immutable.SortedMap
import io.taig.otter.syntax.JsonSyntax.*
import io.taig.otter.syntax.AnnotatedSyntax.*
import io.taig.otter.JsonSchemaExpression
import io.taig.otter.Json
import io.taig.otter.JsonSchemaKeys
import io.taig.otter.Keys
import io.taig.otter.Metadata
import io.taig.otter.Primitive
import io.circe.JsonObject

final class JsonSchemaRenderer(encoder: Encoder[Json, CirceJson])
    extends Renderer[Json, State[SortedMap[String, CirceJson], JsonSchemaExpression]]:
  val record = RecordRenderer(renderer = JsonSchemaFieldRenderer(renderer = this, encoder))

  override def render[A](json: Json[A]): State[SortedMap[String, CirceJson], JsonSchemaExpression] = State:
    definitions =>
      json.attr(JsonSchemaKeys.name).orElse(json.attr(Keys.name)) match
        case Some(name) =>
          definitions.get(name) match
            case Some(definition) =>
              (
                definitions,
                JsonSchemaExpression.Reference(name, data = JsonObject.empty)
              )
            case None =>
              val (update, expression) = renderInline(json).run(definitions).value
              (
                (definitions ++ update).updated(name, expression),
                JsonSchemaExpression.Reference(name, data = JsonObject.empty)
              )
        case None =>
          val (update, expression) = renderInline(json).run(definitions).value.map(JsonSchemaExpression.Inline.apply)
          (definitions ++ update, expression)

  def renderInline[A](json: Json[A]): State[SortedMap[String, CirceJson], CirceJson] = json match
    case Json.Coerce(annotation) =>
      State.pure(render(metadata = annotation.metadata).deepMerge(CirceJson.obj("type" := "string")))
    case Json.Collection(annotation) =>
      render(json = annotation.self.schema.value).map: expression =>
        render(metadata = annotation.metadata).deepMerge(
          CirceJson.obj("type" := "array", "items" := expression)
        )

    case json @ Json.Constant(annotation) =>
      State.pure(render(metadata = annotation.metadata).deepMerge(CirceJson.obj("const" := json.encode(encoder))))
    case Json.Dictionary(annotation) =>
      render(json = annotation.self.schema.value).map: expression =>
        render(metadata = annotation.metadata).deepMerge(
          CirceJson.obj("type" := "object", "additionalProperties" := expression)
        )
    case json @ Json.Enumeration(annotation) =>
      renderInline(json = annotation.self.schema.value).map: data =>
        render(metadata = annotation.metadata)
          .deepMerge(data)
          .deepMerge(CirceJson.obj("enum" := json.encode(encoder)))
    case json @ Json.Nullable(annotation) =>
      renderInline(json = annotation.self.schema.value).map: data =>
        render(metadata = annotation.metadata)
          .deepMerge(data)
          .deepMerge(CirceJson.obj("default" := json.encode(encoder)).dropNullValues)
    case Json.Primitive.Boolean(annotation) =>
      State.pure(render(metadata = annotation.metadata).deepMerge(CirceJson.obj("type" := "boolean")))
    case Json.Primitive.Number(annotation) =>
      State.pure(
        render(metadata = annotation.metadata).deepMerge(CirceJson.obj("type" := tpe(schema = annotation.self)))
      )
    case Json.Primitive.String(annotation) =>
      State.pure(render(metadata = annotation.metadata).deepMerge(CirceJson.obj("type" := "string")))
    case json @ Json.Record(annotation) =>
      record
        .render(annotation.self)
        .map: properties =>
          render(metadata = annotation.metadata).deepMerge(
            CirceJson
              .obj(
                "properties" := CirceJson.fromFields(properties.map((key, value) => key := value).toIterable),
                "required" := json.fields
                  .map(_.value)
                  .mapFilter(field => Option.when(!field.isOptional)(field.name)),
                "type" := "object"
              )
          )
    case Json.Tuple(annotation) =>
      annotation.self.schemas
        .map(_.value)
        .traverse(render)
        .map: expressions =>
          render(metadata = annotation.metadata).deepMerge(
            CirceJson.obj(
              "type" := "array",
              "minItems" := annotation.self.size,
              "prefixItems" := expressions,
              "items" := false
            )
          )
    case Json.Union(annotation) =>
      annotation.self.schemas
        .map(_.value)
        .traverse(render)
        .map: expressions =>
          render(metadata = annotation.metadata)
            .deepMerge(CirceJson.obj("oneOf" := expressions))

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

object JsonSchemaRenderer:
  def apply(encoder: Encoder[Json, CirceJson]): Renderer[Json, CirceJson] =
    new JsonSchemaRenderer(encoder).map: state =>
      val (definitions, expression) = state.run(SortedMap.empty).value

      expression.asJson.deepMerge(
        CirceJson.obj("$defs" := definitions).dropEmptyValues
      )
