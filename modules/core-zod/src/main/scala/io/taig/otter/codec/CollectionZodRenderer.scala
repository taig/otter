package io.taig.otter.codec

import io.taig.otter.ZodState
import io.taig.otter.ZodExpression
import io.taig.otter.Collection
import cats.syntax.all.*

final class CollectionZodRenderer[S[_]](renderer: Renderer[S, ZodState[ZodExpression]])
    extends Renderer[Collection[S, *], ZodState[String]]:
  override def render[A](schema: Collection[S, A]): ZodState[String] =
    renderer.render(schema = schema.schema.value).map(expression => show"z.array($expression)")
