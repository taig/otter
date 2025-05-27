package io.taig.otter.codec

import io.taig.otter.Typescript
import io.taig.otter.http.Query

object QueryTypescriptRenderer extends Renderer[Query, Typescript]:
  override def render[A](schema: Query[A]): Typescript =
    val value = Typescript.String // TODO
    if schema.isOptional then Typescript.Nullable(value) else value
