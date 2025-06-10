package io.taig.otter.codec

import io.taig.otter.TypescriptZodState
import io.taig.otter.TypescriptZod
import io.taig.otter.operation.SchemaInvariant
import io.taig.otter.syntax.EnrichedSyntax.*
import io.taig.otter.TypescriptKeys
import io.taig.otter.TypescriptZodKeys
import io.taig.otter.Typescript
import cats.data.State

final class MetadataTypescriptZodRenderer[S[_]: SchemaInvariant](
    renderer: Renderer[S, TypescriptZodState[TypescriptZod]]
) extends Renderer[S, TypescriptZodState[TypescriptZod]]:
  override def render[A](schema: S[A]): TypescriptZodState[TypescriptZod] =
    (schema.metadata.get(TypescriptKeys.typescript), schema.metadata.get(TypescriptZodKeys.zod)) match
      case (Some(typescript), Some(zod)) => State.pure(TypescriptZod.Split(typescript, zod))
      case (None, Some(zod)) =>
        renderer.render(schema).map(typescript => TypescriptZod.Split(typescript = typescript.toTypescript, zod))
      case (Some(typescript), None) =>
        renderer.render(schema).map(zod => TypescriptZod.Split(typescript, zod = zod.toZod))
      case (None, None) => renderer.render(schema)
