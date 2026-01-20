package io.taig.otter.codec

import cats.data.State
import io.taig.otter.Json
import io.taig.otter.JsonZod
import io.taig.otter.Keys
import io.taig.otter.Zod

import scala.collection.immutable.ListMap
import io.taig.otter.Typescript
import io.taig.otter.Metadata

object JsonZodTypescriptExpressionsRenderer
    extends Renderer[Json.Read, State[ListMap[String, Typescript.Expression], Typescript.Expression]]:
  val renderer = JsonZodTypescriptExpressionRenderer(renderer = this)

  override def render[A](json: Json.Read[A]): State[ListMap[String, Typescript.Expression], Typescript.Expression] =
    State: definitions =>
      json.attr(JsonZod.Namespace, Zod.Namespace, Metadata.Namespace.Global)(key = Keys.name) match
        case Some(name) =>
          definitions.get(name) match
            case Some(_) => (definitions, Typescript.Expression.Identifier(name))
            case None    =>
              val (update, expression) = renderer.render(json).run(definitions).value
              (
                (definitions ++ update).updated(name, expression),
                Typescript.Expression.Identifier(name)
              )
        case None =>
          val (update, expression) = renderer.render(json).run(definitions).value
          (definitions ++ update, expression)
