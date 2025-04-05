package io.taig.otter

import cats.data.State
import io.taig.otter.ZodKeys.*

/** Render the given codec to an inline value, or use the `zod` metadata if present */
final class ZodRenderer[S[_]: CodecInvariant](renderer: Renderer[S, ZodState[String]])
    extends Renderer[S, ZodState[String]]:
  override def apply[A](codec: S[A]): ZodState[String] = State: state =>
    codec.metadata.get(zod) match
      case Some(zod) => (state, zod)
      case None      => renderer(codec).run(initial = state).value

object ZodRenderer:
  def apply[S[_]: CodecInvariant](renderer: Renderer[S, ZodState[String]]): Renderer[S, ZodState[String]] =
    new ZodRenderer(renderer)
