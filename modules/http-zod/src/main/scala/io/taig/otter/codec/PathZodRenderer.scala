package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.http.Parameter
import io.taig.otter.http.Path
import io.taig.otter.indent

object PathZodRenderer extends Renderer[Path, Option[String]]:
  override def render[A](schema: Path[A]): Option[String] =
    val parameters = schema.toSegments.collect { case paramater: Parameter[?] => paramater }

    Option.when(parameters.nonEmpty):
      val fields = parameters.map(parameter => show""""${parameter.name}": ${ParameterZodRenderer.render(parameter)}""")

      s"""z.object({
         |${indent(fields.mkString_(",\n"))}
         |})""".stripMargin
