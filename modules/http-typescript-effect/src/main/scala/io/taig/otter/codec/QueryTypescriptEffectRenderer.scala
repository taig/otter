package io.taig.otter.codec

import io.taig.otter.Effect
import io.taig.otter.TypescriptEffect
import io.taig.otter.http.Query

object QueryTypescriptEffectRenderer extends Renderer[Query, TypescriptEffect]:
  override def render[A](schema: Query[A]): TypescriptEffect =
    val value = TypescriptEffect(Effect.String)
    if schema.isOptional then TypescriptEffect(Effect.Nullable(value)) else value
