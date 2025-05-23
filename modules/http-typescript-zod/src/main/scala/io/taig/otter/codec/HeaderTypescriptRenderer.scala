package io.taig.otter.codec

import io.taig.otter.http.Header
import io.taig.otter.Typescript

object HeaderTypescriptRenderer extends Renderer[Header, Typescript]:
  override def render[A](schema: Header[A]): Typescript =
    val value = Typescript.String // TODO schema.schema.value
    if schema.isOptional then Typescript.Nullable(value) else value
