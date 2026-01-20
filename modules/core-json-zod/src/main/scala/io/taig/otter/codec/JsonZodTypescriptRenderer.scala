package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Typescript
import scala.collection.immutable.ListMap

val JsonZodTypescriptRenderer: Renderer[Json.Read, List[Typescript]] =
  JsonZodTypescriptExpressionsRenderer
    .map(_.run(ListMap.empty).value)
    .map: (state, expression) =>
      state.map((name, expression) => Typescript.Statement.Declaration.Constant(name, expression)).toList :+
        expression
