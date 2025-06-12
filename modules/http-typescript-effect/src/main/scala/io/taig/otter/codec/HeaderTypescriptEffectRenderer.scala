package io.taig.otter.codec

import io.taig.otter.http.Header
import io.taig.otter.TypescriptEffect
import io.taig.otter.Effect

object HeaderTypescriptEffectRenderer extends Renderer[Header, TypescriptEffect]:
  override def render[A](schema: Header[A]): TypescriptEffect =
    val value = TypescriptEffect(Effect.String)

    if schema.isOptional then TypescriptEffect(Effect.Nullable(value)) else value
