package io.taig.otter.codec

import io.taig.otter.Typescript
import io.taig.otter.http.Query
import io.taig.otter.TypescriptZod

object QueryTypescriptZodRenderer extends Renderer[Query, TypescriptZod]:
  override def render[A](schema: Query[A]): TypescriptZod =
    val value = TypescriptZod.Shared(Typescript.String)
    if schema.isOptional then TypescriptZod.Shared(Typescript.Nullable(value)) else value
