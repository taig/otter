package io.taig.otter.codec
import io.taig.otter.Keys
import io.taig.otter.Typescript
import io.taig.otter.http.Headers

object HeadersTypescriptRenderer extends Renderer[Headers, Option[Typescript]]:
  override def render[A](schema: Headers[A]): Option[Typescript] =
    val values = schema.toChain.filter: header =>
      !header.metadata.get(Keys.hidden).getOrElse(false)

    Option
      .when(values.nonEmpty)(values.map(header => (header.name.toString, HeaderTypescriptRenderer.render(header))))
      .map(Typescript.Object.apply)
