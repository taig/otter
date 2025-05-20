package io.taig.otter.codec

import io.taig.otter.http.Headers
import cats.syntax.all.*
import io.taig.otter.indent

object HeadersZodRenderer extends Renderer[Headers, String]:
  override def render[A](schema: Headers[A]): String =
    val values = schema.toChain.map(header => show""""${header.name}": ${HeaderZodRenderer.render(header)}""")
    s"""z.object({
       |${indent(values.mkString_(",\n"))}
       |})""".stripMargin
