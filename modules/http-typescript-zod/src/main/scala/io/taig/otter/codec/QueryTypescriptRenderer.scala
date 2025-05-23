package io.taig.otter.codec

import io.taig.otter.http.Query
import io.taig.otter.Typescript

object QueryTypescriptRenderer extends Renderer[Query, Typescript]:
  override def render[A](schema: Query[A]): Typescript =
    val value = Typescript.String // TODO
    if schema.isOptional then Typescript.Nullable(value) else value
