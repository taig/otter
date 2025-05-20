package io.taig.otter.codec

import io.taig.otter.http.Queries
import cats.syntax.all.*
import io.taig.otter.indent

object QueriesZodRenderer extends Renderer[Queries, Option[String]]:
  override def render[A](schema: Queries[A]): Option[String] =
    val values = schema.toChain

    Option.when(values.nonEmpty):
      val fields = schema.toChain.map(query => show""""${query.name}": ${QueryZodRenderer.render(query)}""")

      s"""z.object({
         |${indent(fields.mkString_(",\n"))}
         |})""".stripMargin
