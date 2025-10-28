package io.taig.otter.codec

import cats.data.State
import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.circe.JsonObject
import io.taig.otter.Json
import io.taig.otter.JsonSchemaExpression
import io.taig.otter.JsonSchemaKeys
import io.taig.otter.Keys
import io.taig.otter.syntax.AnnotatedSyntax.*

import scala.collection.immutable.SortedMap

final class JsonSchemaExpressionRenderer(encoder: Encoder[Json, CirceJson])
    extends Renderer[Json, State[SortedMap[String, CirceJson], JsonSchemaExpression]]:
  val self = JsonSchemaInlineRenderer(encoder, renderer = this)

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
              val (update, expression) = self.render(json).run(definitions).value
              (
                (definitions ++ update).updated(name, expression),
                JsonSchemaExpression.Reference(name, data = JsonObject.empty)
              )
        case None =>
          val (update, expression) = self.render(json).run(definitions).value.map(JsonSchemaExpression.Inline.apply)
          (definitions ++ update, expression)

object JsonSchemaExpressionRenderer:
  def apply(
      encoder: Encoder[Json, CirceJson]
  ): Renderer[Json, State[SortedMap[String, CirceJson], JsonSchemaExpression]] =
    new JsonSchemaExpressionRenderer(encoder)
