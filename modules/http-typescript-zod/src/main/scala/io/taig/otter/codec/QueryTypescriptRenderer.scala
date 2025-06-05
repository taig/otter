package io.taig.otter.codec

import io.taig.otter.Typescript
import io.taig.otter.http.Query
import io.taig.otter.TypescriptZod
import io.taig.otter.Zod

object QueryTypescriptRenderer extends Renderer[Query, TypescriptZod]:
  override def render[A](schema: Query[A]): TypescriptZod =
    val value = TypescriptZod(typescript = Typescript.String, zod = Zod.Expression("z.string()"))

    if schema.isOptional
    then TypescriptZod(typescript = Typescript.Nullable(value.typescript), zod = Zod.Nullable(value.zod))
    else value
