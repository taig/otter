package io.taig.otter.codec

import cats.data.State
import cats.syntax.all.*
import io.taig.otter.Keys
import io.taig.otter.Typescript
import io.taig.otter.operation.SchemaInvariant
import io.taig.otter.syntax.EnrichedSyntax.*
import io.taig.otter.toSymbol
import io.taig.otter.TypescriptState
import io.taig.otter.TypescriptZod
import io.taig.otter.TypescriptZodState

final class TypescriptZodStateRenderer[S[_]: SchemaInvariant](
    renderer: Renderer[S, TypescriptZodState[TypescriptZod]]
) extends Renderer[S, TypescriptZodState[TypescriptZod]]:
  // TODO recursion marker
  override def render[B](schema: S[B]): TypescriptZodState[TypescriptZod] =
    schema.metadata(Keys.name).map(toSymbol) match
      case Some(name) =>
        State:
          state =>
            if state.stack.contains_(name)
            then (state, TypescriptZod.Shared(Typescript.Reference(name)))
            else
              state.references.get(name) match
                case Some(value) => (state, TypescriptZod.Shared(Typescript.Reference(name)))
                case None =>
                  val (update, typescript) = renderer.render(schema).run(initial = state.push(name)).value
                  ???
                  // (
                  //   update.modifyReferences(_.updatedWith(name)(_ => TypescriptZodState.Reference.Shared(typescript).some)).pop(name),
                  //   TypescriptZod.Shared(Typescript.Reference(name))
                  // )
      case None => renderer.render(schema)
