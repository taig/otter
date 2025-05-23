package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.http.Queries
import io.taig.otter.indent
import io.taig.otter.Typescript

object QueriesTypescriptRenderer extends Renderer[Queries, Option[Typescript]]:
  override def render[A](schema: Queries[A]): Option[Typescript] =
    val values = schema.toChain

    Option
      .when(values.nonEmpty)(values.map(query => (query.name, QueryTypescriptRenderer.render(query))))
      .map(Typescript.Object.apply)
