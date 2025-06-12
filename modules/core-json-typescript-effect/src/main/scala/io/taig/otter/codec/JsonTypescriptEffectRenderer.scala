package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.TypescriptEffectState
import io.taig.otter.TypescriptEffect
import cats.syntax.all.*

object JsonTypescriptEffectRenderer extends Renderer[Json, TypescriptEffectState[TypescriptEffect]]:
  val renderer = ReferenceTypescriptEffectRenderer(
    renderer = JsonEffectRenderer(value = this).map(_.map(TypescriptEffect(_)))
  )

  override def render[A](schema: Json[A]): TypescriptEffectState[TypescriptEffect] = renderer.render(schema)

  // JsonEffectRenderer.State
  //   .render(schema)
  //   .transformS[ContextState.Context[TypescriptEffect]](
  //     _.map(_.effect),
  //     (context, updates) =>
  //       ContextState.Context(
  //         references = updates.references.foldLeft(context.references) { case (result, (name, effect)) =>
  //           result.updatedWith(name):
  //             case Some(typescript) => Some(typescript.copy(effect = effect))
  //             case None             => Some(TypescriptEffect(typescript = none, effect))
  //         },
  //         stack = context.stack
  //       )
  //   )
  //   .map: effect =>
  //     val isRecursive = true
  //     if isRecursive
  //     then
  //       val typescript = JsonTypescriptRenderer[Id].render(schema)
  //       TypescriptEffect(typescript = typescript.some, effect)
  //     else TypescriptEffect(typescript = none, effect)
