package io.taig.otter.codec

import io.taig.otter.syntax.EnrichedSyntax.*
import io.taig.otter.operation.SchemaInvariant
import io.taig.otter.Metadata
import cats.syntax.all.*
import cats.Applicative

final class MetadataRenderer[S[_]: SchemaInvariant, V[_]: Applicative, A](renderer: Renderer[S, V[A]])(
    key: Metadata.Key[A]
) extends Renderer[S, V[A]]:
  override def render[B](schema: S[B]): V[A] = schema.metadata.get(key).fold(renderer.render(schema))(_.pure)
