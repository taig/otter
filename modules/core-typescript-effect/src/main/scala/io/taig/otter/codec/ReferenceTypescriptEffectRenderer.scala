package io.taig.otter.codec

import io.taig.otter.Effect
import io.taig.otter.operation.SchemaInvariant
import io.taig.otter.syntax.EnrichedSyntax.*
import io.taig.otter.Keys
import cats.data.State
import cats.syntax.all.*
import io.taig.otter.TypescriptEffectState
import io.taig.otter.TypescriptEffect
import io.taig.otter.Typescript

final class ReferenceTypescriptEffectRenderer[S[_]: SchemaInvariant](
    renderer: Renderer[S, TypescriptEffectState[TypescriptEffect]],
    typescript: Renderer[S, Typescript.Value]
) extends Renderer[S, TypescriptEffectState[TypescriptEffect]]:
  override def render[B](schema: S[B]): TypescriptEffectState[TypescriptEffect] =
    schema.metadata.get(Keys.name).map(toSymbol) match
      case Some(name) =>
        State: state =>
          val reference = TypescriptEffect(Effect.Reference(name))

          if state.stack.contains_(name)
          then (state.recurse(name), TypescriptEffect(Effect.Recursion(name, reference)))
          else
            state.references.get(name) match
              case Some(current) => (state, current)
              case None =>
                val (context, effect) = renderer.render(schema).run(initial = state.push(name)).value
                val updatedEffect = if context.recursion.nonEmpty
                  then effect.copy(typescript = typescript.render(schema).some)
                  else effect
                (context.modifyReferences(_.updatedWith(name)(_ => updatedEffect.some)).pop(name), reference)
      case None => renderer.render(schema)

  private def toSymbol(value: String): String = value.replace(".", "").replace(" ", "")
