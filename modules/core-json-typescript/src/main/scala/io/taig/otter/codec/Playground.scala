package io.taig.otter.codec

import io.taig.otter.component.JsonComponent.*
import io.taig.otter.ContextState
import io.taig.otter.Typescript
import io.taig.otter.syntax.AllTypescriptSyntax.*

object Playground:
  @main
  def run = {
    val renderer = JsonTypescriptRenderer.map(_.map(Typescript.Value.apply))
    val schema = collection.list(string.typescript("MyForbiddenString"))
    val typescript = renderer.render(schema).runA(ContextState.Context.Empty).value
    val output = TypescriptPrinter.print(typescript)
    println(output)
  }
