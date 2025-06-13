package io.taig.otter.codec

import io.taig.otter.Keys
import io.taig.otter.Typescript
import io.taig.otter.operation.SchemaInvariant
import io.taig.otter.syntax.EnrichedSyntax.*

final class ReferenceTypescriptRenderer[S[_]: SchemaInvariant, A](renderer: Renderer[S, A])(lift: Typescript[A] => A)
    extends Renderer[S, A]:
  override def render[B](schema: S[B]): A =
    schema.metadata.get(Keys.name).map(toSymbol).fold(renderer.render(schema))(name => lift(Typescript.Reference(name)))

  private def toSymbol(value: String): String = value.replace(".", "").replace(" ", "")
