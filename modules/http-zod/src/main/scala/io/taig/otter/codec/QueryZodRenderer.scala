package io.taig.otter.codec

import io.taig.otter.http.Query

object QueryZodRenderer extends Renderer[Query, String]:
  override def render[A](schema: Query[A]): String =
    val value = "z.string()" // TODO
    if schema.isOptional then s"z.optional($value)" else value
