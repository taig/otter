package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Typescript

val JsonTypescriptEffectRenderer: Renderer[Json.Write, List[Typescript]] =
  JsonStateTypescriptExpressionEffectRenderer
    .map(_.runEmpty.value)
    .map((context, expression) => context.declarations :+ expression)
