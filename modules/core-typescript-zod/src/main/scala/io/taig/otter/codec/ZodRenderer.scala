package io.taig.otter.codec

import cats.data.State
import io.taig.otter.ZodKeys.*
import io.taig.otter.ZodState
import io.taig.otter.schema.EnrichedSchema

/** Render the given codec to an inline value, or use the `zod` metadata if present */
final class ZodRenderer[S[_]: EnrichedSchema](renderer: Renderer[S, ZodState[String]])
    extends Renderer[S, ZodState[String]]:
  override def render[A](schema: S[A]): ZodState[String] =
    schema.metadata(zod).fold(renderer.render(schema))(State.pure)
