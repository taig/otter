package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.TypescriptZodState
import io.taig.otter.syntax.EnrichedSyntax.*
import io.taig.otter.TypescriptZod
import io.taig.otter.TypescriptKeys
import io.taig.otter.TypescriptZodKeys
import io.taig.otter.ContextState
import cats.data.IndexedState
import io.taig.otter.Typescript
import cats.data.State

final class JsonTypescriptMetadataZodRenderer(renderer: Renderer[Json, TypescriptZodState[TypescriptZod]])
    extends Renderer[Json, TypescriptZodState[TypescriptZod]]:
  override def render[A](schema: Json[A]): TypescriptZodState[TypescriptZod] =
    (schema.metadata.get(TypescriptKeys.typescript), schema.metadata.get(TypescriptZodKeys.zod)) match
      case (Some(typescript), Some(zod)) =>
        TypescriptZod
          .Split(
            typescript = Typescript.Dynamic(typescript),
            zod = Typescript.Dynamic(typescript)
          )
          .pure
      case (None, Some(zod)) =>
        renderer
          .render(schema)
          .map:
            case TypescriptZod.Shared(self) => TypescriptZod.Split(typescript = self, zod = Typescript.Dynamic(zod))
            case TypescriptZod.Split(typescript, _) => TypescriptZod.Split(typescript, zod = Typescript.Dynamic(zod))
            case TypescriptZod.Type(self)    => TypescriptZod.Split(typescript = self, zod = Typescript.Dynamic(zod))
            case TypescriptZod.Expression(_) => TypescriptZod.Expression(self = Typescript.Dynamic(zod))
      case (Some(typescript), None) =>
        renderer
          .render(schema)
          .map:
            case TypescriptZod.Shared(self) =>
              TypescriptZod.Split(typescript = Typescript.Dynamic(typescript), zod = self)
            case TypescriptZod.Split(_, zod) => TypescriptZod.Split(typescript = Typescript.Dynamic(typescript), zod)
            case TypescriptZod.Type(_)       => TypescriptZod.Type(self = Typescript.Dynamic(typescript))
            case TypescriptZod.Expression(self) =>
              TypescriptZod.Split(typescript = Typescript.Dynamic(typescript), zod = self)
      case (None, None) => renderer.render(schema)
