package io.taig.otter.codec

import io.taig.otter.Effect
import io.taig.otter.EffectState
import io.taig.otter.operation.SchemaInvariant
import io.taig.otter.syntax.EnrichedSyntax.*
import io.taig.otter.Keys
import cats.data.State
import cats.syntax.all.*
import io.taig.otter.ContextState

final class ReferenceEffectRenderer[S[_]: SchemaInvariant, A](renderer: Renderer[S, EffectState[A]])
    extends Renderer[S, EffectState[A]]:
  override def render[B](schema: S[B]): EffectState[A] = schema.metadata.get(Keys.name) match
    case Some(name) =>
      State: state =>
        state.references.get(name) match
          case Some(_) => (state, Effect.Reference(name))
          case None =>
            val (context, effect) = renderer.render(schema).run(initial = state.push(name)).value

            (
              context.modifyReferences(_.updatedWith(name)(_ => effect.some)).pop(name),
              Effect.Reference(name)
            )
    case None => renderer.render(schema)
