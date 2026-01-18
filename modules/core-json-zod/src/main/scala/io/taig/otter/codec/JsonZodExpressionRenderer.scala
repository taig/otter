package io.taig.otter.codec

import cats.syntax.all.*
import cats.data.State
import io.taig.otter.Json

import scala.collection.immutable.ListMap
import io.taig.otter.JsonZodExpression
import io.taig.otter.Keys
import io.taig.otter.JsonZod
import io.taig.otter.Zod

object JsonZodExpressionRenderer extends Renderer[Json.Read, State[ListMap[String, String], JsonZodExpression]]:
  val renderer = JsonZodInlineRenderer(renderer = this)

  override def render[A](json: Json.Read[A]): State[ListMap[String, String], JsonZodExpression] =
    State: definitions =>
      json.attr(JsonZod.Key.name, Zod.Key.name, Keys.name) match
        case Some(name) =>
          definitions.get(name) match
            case Some(_) => (definitions, JsonZodExpression.Reference(name))
            case None    =>
              val (update, expression) = renderer.render(json).run(definitions).value
              (
                (definitions ++ update).updated(name, expression),
                JsonZodExpression.Reference(name)
              )
        case None =>
          val (update, expression) = renderer.render(json).run(definitions).value.map(JsonZodExpression.Inline.apply)
          (definitions ++ update, expression)
