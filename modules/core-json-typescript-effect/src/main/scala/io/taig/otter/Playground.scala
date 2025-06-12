package io.taig.otter

import io.taig.otter.component.JsonComponent.*
import io.taig.otter.codec.JsonTypescriptEffectRenderer
import cats.syntax.all.*

object Playground:
  @main
  def run = {
    val schema = data.any
    val (context, result) = JsonTypescriptEffectRenderer.render(schema).run(initial = ContextState.Context.Empty).value
    println(context.references.toList.map(TypescriptEffectDefinition(_, _)).mkString_("\n\n"))
    println(result.toEffect.show)
  }
