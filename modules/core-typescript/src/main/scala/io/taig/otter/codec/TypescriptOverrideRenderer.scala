package io.taig.otter.codec

import io.taig.otter.operation.SchemaInvariant
import io.taig.otter.syntax.EnrichedSyntax.*
import io.taig.otter.Metadata
import cats.syntax.all.*
import cats.Applicative
import io.taig.otter.Typescript

final class TypescriptOverrideRenderer[S[_]: SchemaInvariant, T[_]: Applicative, A](
    renderer: Renderer[S, T[Typescript[A]]]
)(key: Metadata.Key[Typescript[A]])
    extends Renderer[S, T[Typescript[A]]]:
  override def render[B](schema: S[B]): T[Typescript[A]] = schema.metadata.get(key) match
    case Some(a) => a.pure
    case None    => renderer.render(schema)
