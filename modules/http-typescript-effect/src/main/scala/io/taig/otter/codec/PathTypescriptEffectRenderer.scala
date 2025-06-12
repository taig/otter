package io.taig.otter.codec

import io.taig.otter.Keys
import io.taig.otter.http.Parameter
import io.taig.otter.http.Path
import io.taig.otter.TypescriptEffect
import io.taig.otter.Effect

object PathTypescriptRenderer extends Renderer[Path, Option[TypescriptEffect]]:
  override def render[A](schema: Path[A]): Option[TypescriptEffect] =
    val parameters = schema.toSegments
      .collect { case paramater: Parameter[?] => paramater }
      .filter: parameter =>
        !parameter.metadata.get(Keys.hidden).getOrElse(false)

    Option
      .when(parameters.nonEmpty)(
        parameters.map(parameter => (parameter.name, ParameterTypescriptEffectRenderer.render(parameter)))
      )
      .map(values => TypescriptEffect(Effect.Object(values.map((name, value) => (name, value)))))
