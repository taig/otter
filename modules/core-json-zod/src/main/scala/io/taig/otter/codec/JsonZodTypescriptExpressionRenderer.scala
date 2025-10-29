package io.taig.otter.codec

import cats.data.State
import io.taig.otter.Json
import io.taig.otter.Keys
import io.taig.otter.ZodKeys
import cats.syntax.all.*
import io.taig.otter.ZodTypescriptExpression
import io.taig.otter.syntax.AnnotatedSyntax.*

import scala.collection.immutable.ListMap

object JsonZodTypescriptExpressionRenderer
    extends Renderer[Json, State[ListMap[String, String], ZodTypescriptExpression]]:
  val self = JsonZodInlineRenderer(renderer = this)

  override def render[A](json: Json[A]): State[ListMap[String, String], ZodTypescriptExpression] = State: definitions =>
    json.attr(ZodKeys.name).orElse(json.attr(Keys.name)) match
      case Some(name) =>
        definitions.get(name) match
          case Some(definition) =>
            (definitions, ZodTypescriptExpression.Reference(name))
          case None =>
            val (update, expression) = self.render(json).run(definitions).value
            (
              (definitions ++ update).updated(name, expression),
              ZodTypescriptExpression.Reference(name)
            )
      case None =>
        val (update, expression) = self.render(json).run(definitions).value.map(ZodTypescriptExpression.Inline.apply)
        (definitions ++ update, expression)
