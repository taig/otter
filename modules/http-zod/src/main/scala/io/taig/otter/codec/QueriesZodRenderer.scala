package io.taig.otter.codec

import io.taig.otter.http.Queries
import cats.syntax.all.*
import io.taig.otter.indent

object QueriesZodRenderer extends Renderer[Queries, String]:
  override def render[A](schema: Queries[A]): String =
    val values = schema.toChain.map(query => show""""${query.name}": ${QueryZodRenderer.render(query)}""")
    s"""z.object({
       |${indent(values.mkString_(",\n"))}
       |})""".stripMargin
