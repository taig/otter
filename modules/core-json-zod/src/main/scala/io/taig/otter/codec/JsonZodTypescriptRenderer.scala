package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Typescript

val JsonZodTypescriptRenderer: Renderer[Json.Read, List[Typescript]] = JsonZodTypescriptExpressionsRenderer
  .map(_.runEmpty.value)
  .map((context, expression) => context.declarations :+ expression)
