package io.taig.otter.codec
import io.taig.otter.Keys
import io.taig.otter.Typescript
import io.taig.otter.http.Parameter
import io.taig.otter.http.Path

object PathTypescriptRenderer extends Renderer[Path, Option[Typescript]]:
  override def render[A](schema: Path[A]): Option[Typescript] =
    val parameters = schema.toSegments
      .collect { case paramater: Parameter[?] => paramater }
      .filter: parameter =>
        !parameter.metadata.get(Keys.hidden).getOrElse(false)

    Option
      .when(parameters.nonEmpty)(
        parameters.map(parameter => (parameter.name, ParameterTypescriptRenderer.render(parameter)))
      )
      .map(Typescript.Object.apply)
