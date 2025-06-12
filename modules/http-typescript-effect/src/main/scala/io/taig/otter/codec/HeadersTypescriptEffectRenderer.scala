package io.taig.otter.codec

import io.taig.otter.Keys
import io.taig.otter.Typescript
import io.taig.otter.http.Headers
import io.taig.otter.TypescriptEffect
import io.taig.otter.Effect

object HeadersTypescriptEffectRenderer extends Renderer[Headers, Option[TypescriptEffect]]:
  override def render[A](schema: Headers[A]): Option[TypescriptEffect] =
    val values = schema.toChain.filter: header =>
      !header.metadata.get(Keys.hidden).getOrElse(false)

    Option
      .when(values.nonEmpty)(
        values.map(header => (header.name.toString, HeaderTypescriptEffectRenderer.render(header)))
      )
      .map(values => TypescriptEffect(Effect.Object(values.map((name, value) => (name, value)))))
