package io.taig.otter.codec

import io.taig.otter.Typescript
import io.taig.otter.http.Parameter
import io.taig.otter.TypescriptEffect
import io.taig.otter.Effect

object ParameterTypescriptEffectRenderer extends Renderer[Parameter, TypescriptEffect]:
  override def render[A](schema: Parameter[A]): TypescriptEffect = TypescriptEffect(Effect.String)
