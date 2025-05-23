package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.http.Headers
import io.taig.otter.indent
import io.taig.otter.Typescript

object HeadersTypescriptRenderer extends Renderer[Headers, Option[Typescript]]:
  override def render[A](schema: Headers[A]): Option[Typescript] =
    val values = schema.toChain

    Option
      .when(values.nonEmpty)(values.map(header => (header.name.toString, HeaderTypescriptRenderer.render(header))))
      .map(Typescript.Object.apply)
