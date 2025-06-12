package io.taig.otter.codec

import io.taig.otter.operation.SchemaInvariant
import io.taig.otter.Typescript
import io.taig.otter.syntax.EnrichedSyntax.*
import io.taig.otter.Keys

final class ReferenceTypescriptRenderer[S[_]: SchemaInvariant, A](renderer: Renderer[S, Typescript[A]])
    extends Renderer[S, Typescript[A]]:
  override def render[B](schema: S[B]): Typescript[A] =
    schema.metadata.get(Keys.name).map(toSymbol).fold(renderer.render(schema))(Typescript.Reference.apply)

  private def toSymbol(value: String): String = value.replace(".", "").replace(" ", "")
