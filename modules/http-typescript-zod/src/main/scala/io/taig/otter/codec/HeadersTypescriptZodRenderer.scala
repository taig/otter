package io.taig.otter.codec

import io.taig.otter.Keys
import io.taig.otter.Typescript
import io.taig.otter.http.Headers
import io.taig.otter.TypescriptZod

object HeadersTypescriptZodRenderer extends Renderer[Headers, Option[TypescriptZod]]:
  override def render[A](schema: Headers[A]): Option[TypescriptZod] =
    val values = schema.toChain.filter: header =>
      !header.metadata.get(Keys.hidden).getOrElse(false)

    Option
      .when(values.nonEmpty)(values.map(header => (header.name.toString, HeaderTypescriptZodRenderer.render(header))))
      .map(values => TypescriptZod.Shared(Typescript.Object(values.map((name, value) => (name, value)))))
