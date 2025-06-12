package io.taig.otter.codec

import io.taig.otter.Keys
import io.taig.otter.Typescript
import io.taig.otter.http.Queries
import io.taig.otter.TypescriptEffect
import io.taig.otter.Effect

object QueriesTypescriptEffectRenderer extends Renderer[Queries, Option[TypescriptEffect]]:
  override def render[A](schema: Queries[A]): Option[TypescriptEffect] =
    val values = schema.toChain.filter: queries =>
      !queries.metadata.get(Keys.hidden).getOrElse(false)

    Option
      .when(values.nonEmpty)(values.map(query => (query.name, QueryTypescriptEffectRenderer.render(query))))
      .map(values => TypescriptEffect(Effect.Struct(values.map((name, value) => (name, value)))))
