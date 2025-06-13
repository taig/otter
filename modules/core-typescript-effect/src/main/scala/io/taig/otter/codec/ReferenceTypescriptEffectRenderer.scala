package io.taig.otter.codec

import cats.data.State
import cats.syntax.all.*
import io.taig.otter.Effect
import io.taig.otter.Keys
import io.taig.otter.Typescript
import io.taig.otter.TypescriptEffect
import io.taig.otter.TypescriptEffectState
import io.taig.otter.operation.SchemaInvariant
import io.taig.otter.syntax.EnrichedSyntax.*
import io.taig.otter.TypescriptKeys
import io.taig.otter.EffectKeys

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
                val (context, effect) = overrideOrRender(schema).run(initial = state.push(name)).value
                val updatedEffect =
                  if context.recursion.nonEmpty
                  then effect.copy(typescript = typescript.render(schema).some)
                  else effect
                (context.modifyReferences(_.updatedWith(name)(_ => updatedEffect.some)).pop(name), reference)
      case None => overrideOrRender(schema)

  def overrideOrRender[B](schema: S[B]): TypescriptEffectState[TypescriptEffect] = 
    renderer.render(schema)
      .map: result =>
        schema.metadata.get(TypescriptKeys.typescript).fold(result)(typescript => result.copy(typescript = typescript.some))
      .map: result =>
        schema.metadata.get(EffectKeys.effect).fold(result)(effect => result.copy(effect = effect))

  private def toSymbol(value: String): String = value.replace(".", "").replace(" ", "")
