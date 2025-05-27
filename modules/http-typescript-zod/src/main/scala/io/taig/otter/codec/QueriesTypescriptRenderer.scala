package io.taig.otter.codec
import io.taig.otter.Typescript
import io.taig.otter.http.Queries

object QueriesTypescriptRenderer extends Renderer[Queries, Option[Typescript]]:
  override def render[A](schema: Queries[A]): Option[Typescript] =
    val values = schema.toChain

    Option
      .when(values.nonEmpty)(values.map(query => (query.name, QueryTypescriptRenderer.render(query))))
      .map(Typescript.Object.apply)
