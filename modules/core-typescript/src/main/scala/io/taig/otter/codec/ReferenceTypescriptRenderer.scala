package io.taig.otter.codec

import cats.data.State
import cats.syntax.all.*
import io.taig.otter.Keys
import io.taig.otter.Typescript
import io.taig.otter.operation.SchemaInvariant
import io.taig.otter.syntax.EnrichedSyntax.*
import io.taig.otter.toSymbol
import io.taig.otter.ContextState

final class ReferenceTypescriptRenderer[S[_]: SchemaInvariant, A](
    renderer: Renderer[S, ContextState[Typescript[A], Typescript[A]]]
) extends Renderer[S, ContextState[Typescript[A], Typescript[A]]]:
  // TODO recursion marker
  override def render[B](schema: S[B]): ContextState[Typescript[A], Typescript[A]] =
    schema.metadata(Keys.name).map(toSymbol) match
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
