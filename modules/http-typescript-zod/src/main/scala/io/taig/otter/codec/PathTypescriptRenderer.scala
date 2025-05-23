package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.http.Parameter
import io.taig.otter.http.Path
import io.taig.otter.indent
import io.taig.otter.Typescript

object PathTypescriptRenderer extends Renderer[Path, Option[Typescript]]:
  override def render[A](schema: Path[A]): Option[Typescript] =
    val parameters = schema.toSegments.collect { case paramater: Parameter[?] => paramater }

    Option
      .when(parameters.nonEmpty)(
        parameters.map(parameter => (parameter.name, ParameterTypescriptRenderer.render(parameter)))
      )
      .map(Typescript.Object.apply)
