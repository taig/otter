package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.TypescriptEffectState
import io.taig.otter.TypescriptEffect
import cats.syntax.all.*
import cats.data.State
import io.taig.otter.ContextState
import scala.collection.immutable.SortedSet
import cats.Id
import io.taig.otter.EffectState
import io.taig.otter.Effect

object JsonTypescriptEffectRenderer extends Renderer[Json, TypescriptEffectState[TypescriptEffect]]:
  val renderer = ReferenceTypescriptEffectRenderer(renderer = this)

  override def render[A](schema: Json[A]): TypescriptEffectState[TypescriptEffect] =
    renderer.render(schema)

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
