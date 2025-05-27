package io.taig.otter.codec

import cats.data.State
import cats.syntax.all.*
import io.taig.otter.Keys.*
import io.taig.otter.Typescript
import io.taig.otter.TypescriptState
import io.taig.otter.operation.EnrichedSchemaInvariant
import io.taig.otter.toSymbol

final class ReferenceTypescriptRenderer[S[_]: EnrichedSchemaInvariant](
    renderer: Renderer[S, TypescriptState[Typescript]]
) extends Renderer[S, TypescriptState[Typescript]]:
  override def render[A](schema: S[A]): TypescriptState[Typescript] = schema.metadata(name).map(toSymbol) match
    case Some(name) =>
      State: state =>
        if state.stack.contains_(name)
        then (state, Typescript.Reference(name))
        else
          state.references.get(name) match
            case Some(value) => (state, Typescript.Reference(name))
            case None =>
              val (update, typescript) = renderer.render(schema).run(initial = state.push(name)).value

              (
                update.modifyReferences(_.updatedWith(name)(_ => typescript.some)).pop(name),
                Typescript.Reference(name)
              )
    case None => renderer.render(schema)
