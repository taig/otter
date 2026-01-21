package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Typescript

val JsonZodTypescriptRenderer: Renderer[Json.Write, List[Typescript]] =
  JsonZodStateTypescriptRenderer
    .map(_.runEmpty.value)
    .map((context, expression) => context.declarations :+ expression)
