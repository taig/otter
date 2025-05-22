package io.taig.otter.codec

import io.taig.otter.component.JsonComponent.*
import io.taig.otter.Json
import io.taig.otter.Keys.*
import io.taig.otter.TypescriptState

object Playground:
  val array: Json.Collection[?] = (collection.list(value)).metadata(name, "Arr")
  val primitive: Json.Union[?] = (boolean | int | string).metadata(name, "Primitive")
  val value: Json.Union[?] = array.:+(primitive).metadata(name, "Value")

  @main
  def run = {
    val (context, ts) = JsonTypescriptRenderer.render(array).run(initial = TypescriptState.Context.Empty).value
    TypecriptReferencesPrinter.print(context.references)
      .foreach(println)
    println(ts)
  }
