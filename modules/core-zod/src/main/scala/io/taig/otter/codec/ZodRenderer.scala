package io.taig.otter.codec

import cats.data.State
import io.taig.otter.ZodKeys.*
import io.taig.otter.schema.Schema
import io.taig.otter.ZodState
import io.taig.otter.ZodExpression

/** Render the given codec to an inline value, or use the `zod` metadata if present */
final class ZodRenderer[S[_]: Schema](renderer: Renderer[S, ZodState[ZodExpression]])
  extends Renderer[S, ZodState[ZodExpression]]:
  override def render[A](schema: S[A]): ZodState[ZodExpression] = State: state =>
    schema.metadata(zod) match
      case Some(zod) => (state, ZodExpression.Inline(zod))
      case None      => renderer.render(schema).run(initial = state).value
