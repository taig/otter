package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.TypescriptEffectState
import io.taig.otter.TypescriptEffect
import cats.syntax.all.*

object JsonTypescriptEffectRenderer extends Renderer[Json, TypescriptEffectState[TypescriptEffect]]:

  override def render[A](schema: Json[A]): TypescriptEffectState[TypescriptEffect] =
    ReferenceTypescriptEffectRenderer(
      renderer = JsonEffectRenderer(value = this).map(_.map(TypescriptEffect.apply))/*.map(_.map { effect =>
        if effect.isRecursion || effect.exists(_.isRecursive)
        then
          TypescriptEffect(
            typescript = JsonTypescriptRenderer
              .render(schema)
              .some,
            effect
          )
        else TypescriptEffect(effect)
      })*/,
      typescript = JsonTypescriptRenderer
    ).render(schema)
