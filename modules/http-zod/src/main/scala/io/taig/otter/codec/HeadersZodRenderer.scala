package io.taig.otter.codec

import io.taig.otter.http.Headers
import cats.syntax.all.*
import io.taig.otter.indent

object HeadersZodRenderer extends Renderer[Headers, Option[String]]:
  override def render[A](schema: Headers[A]): Option[String] =
    val values = schema.toChain

    Option.when(values.nonEmpty):
      val fields = values.map(header => show""""${header.name}": ${HeaderZodRenderer.render(header)}""")

      s"""z.object({
         |${indent(fields.mkString_(",\n"))}
         |})""".stripMargin
