package io.taig.otter

import cats.data.State
import io.taig.otter.ZodKeys.*
import io.taig.otter.codec.Encoder
import io.taig.otter.schema.Schema

/** Render the given codec to an inline value, or use the `zod` metadata if present */
final class ZodRenderer[S[_]: Schema](renderer: Renderer[S, ZodState[String]]) extends Renderer[S, ZodState[String]]:
  override def render[A](schema: S[A]): ZodState[String] = State: state =>
    schema.metadata(zod) match
      case Some(zod) => (state, zod)
      case None      => renderer.render(schema).run(initial = state).value
