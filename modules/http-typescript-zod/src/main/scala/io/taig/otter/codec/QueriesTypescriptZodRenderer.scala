package io.taig.otter.codec

import io.taig.otter.Keys
import io.taig.otter.Typescript
import io.taig.otter.http.Queries
import io.taig.otter.TypescriptZod

object QueriesTypescriptZodRenderer extends Renderer[Queries, Option[TypescriptZod]]:
  override def render[A](schema: Queries[A]): Option[TypescriptZod] =
    val values = schema.toChain.filter: queries =>
      !queries.metadata.get(Keys.hidden).getOrElse(false)

    Option
      .when(values.nonEmpty)(values.map(query => (query.name, QueryTypescriptZodRenderer.render(query))))
      .map(values => TypescriptZod.Shared(Typescript.Object(values.map((name, value) => (name, value)))))
