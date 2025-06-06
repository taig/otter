package io.taig.otter.codec
import io.taig.otter.Keys
import io.taig.otter.Typescript
import io.taig.otter.http.Parameter
import io.taig.otter.http.Path
import io.taig.otter.TypescriptZod

object PathTypescriptRenderer extends Renderer[Path, Option[TypescriptZod]]:
  override def render[A](schema: Path[A]): Option[TypescriptZod] =
    val parameters = schema.toSegments
      .collect { case paramater: Parameter[?] => paramater }
      .filter: parameter =>
        !parameter.metadata.get(Keys.hidden).getOrElse(false)

    Option
      .when(parameters.nonEmpty)(
        parameters.map(parameter => (parameter.name, ParameterTypescriptZodRenderer.render(parameter)))
      )
      .map(values => TypescriptZod.Shared(Typescript.Object(values.map((name, value) => (name, value)))))
