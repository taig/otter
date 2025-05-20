package io.taig.otter.codec

import io.taig.otter.http.Header

object HeaderZodRenderer extends Renderer[Header, String]:
  override def render[A](schema: Header[A]): String =
    val value = "z.string()" // TODO schema.schema.value
    if schema.isOptional then s"z.optional($value)" else value
