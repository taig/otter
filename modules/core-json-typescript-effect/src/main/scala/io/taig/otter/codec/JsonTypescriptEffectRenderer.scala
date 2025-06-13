package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.TypescriptEffectState
import io.taig.otter.TypescriptEffect
import cats.syntax.all.*

object JsonTypescriptEffectRenderer extends Renderer[Json, TypescriptEffectState[TypescriptEffect]]:

  override def render[A](schema: Json[A]): TypescriptEffectState[TypescriptEffect] =
    ReferenceTypescriptEffectRenderer(
      renderer = JsonEffectRenderer(renderer = this)(lift = TypescriptEffect.apply).map(_.map(TypescriptEffect.apply)),
      typescript = JsonTypescriptRenderer
    ).render(schema)
