package io.taig.otter.codec

import io.taig.otter.Typescript
import io.taig.otter.http.Header
import io.taig.otter.TypescriptZod

object HeaderTypescriptZodRenderer extends Renderer[Header, TypescriptZod]:
  override def render[A](schema: Header[A]): TypescriptZod =
    val value = TypescriptZod.Shared(Typescript.String)

    if schema.isOptional then TypescriptZod.Shared(Typescript.Nullable(value)) else value
