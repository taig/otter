package io.taig.otter.codec
import io.taig.otter.Keys
import io.taig.otter.Typescript
import io.taig.otter.http.Queries
import io.taig.otter.TypescriptZod
import io.taig.otter.Zod

object QueriesTypescriptRenderer extends Renderer[Queries, Option[TypescriptZod]]:
  override def render[A](schema: Queries[A]): Option[TypescriptZod] =
    val values = schema.toChain.filter: queries =>
      !queries.metadata.get(Keys.hidden).getOrElse(false)

    Option
      .when(values.nonEmpty)(values.map(query => (query.name, QueryTypescriptRenderer.render(query))))
      .map: values =>
        TypescriptZod(
          typescript = Typescript.Object(values.map((name, value) => (name, value.typescript))),
          zod = Zod.Object(values.map((name, value) => (name, value.zod)))
        )
