package io.taig.otter

import io.taig.otter.component.JsonComponent.*
import io.taig.otter.codec.TypescriptZodPrinter
import io.taig.otter.codec.JsonTypescriptRenderer
import io.taig.otter.Keys.*

object Playground:
  @main
  def run: Unit = {
    val (context, ts) = JsonTypescriptRenderer.render(schema = data.any).run(initial = TypescriptState.Context.Empty).value
    val zod = TypescriptZodPrinter.print(ts)

    context.references.foreach: (name, value) =>
      println(s"type $name = $value")

    println(ts)

    // context.references.foreach(println)
    // println(ts)
    // println(zod)
  }
