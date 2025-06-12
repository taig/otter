package io.taig.otter.codec

import io.taig.otter.Effect
import io.taig.otter.operation.SchemaInvariant
import io.taig.otter.syntax.EnrichedSyntax.*
import io.taig.otter.Keys
import cats.data.State
import cats.syntax.all.*
import io.taig.otter.TypescriptEffectState
import io.taig.otter.TypescriptEffect

final class ReferenceTypescriptEffectRenderer[S[_]: SchemaInvariant](
    renderer: Renderer[S, TypescriptEffectState[TypescriptEffect]]
) extends Renderer[S, TypescriptEffectState[TypescriptEffect]]:
  override def render[B](schema: S[B]): TypescriptEffectState[TypescriptEffect] = schema.metadata.get(Keys.name) match
    case Some(name) =>
      State: state =>
        state.references.get(name) match
          case Some(_) => (state, TypescriptEffect(Effect.Reference(name)))
          case None =>
            val (context, effect) = renderer.render(schema).run(initial = state.push(name)).value

            (
              context.modifyReferences(_.updatedWith(name)(_ => effect.some)).pop(name),
              TypescriptEffect(Effect.Reference(name))
            )
    case None => renderer.render(schema)
