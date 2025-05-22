package io.taig.otter.codec

import io.taig.otter.component.JsonComponent.*
import scala.collection.immutable.ListMap
import io.taig.otter.ZodState
import io.taig.otter.Json
import io.taig.otter.Keys.*
import io.taig.otter.TypescriptState

object Playground:
  val array: Json.Collection[?] = (collection.list(value)).metadata(name, "Array")
  val primitive: Json.Union[?] = boolean | int | string
  val value: Json.Union[?] = array.:+(primitive).metadata(name, "Value")

  @main
  def run = {
    val (references, ts) = JsonTypescriptRenderer.render(value).run(initial = TypescriptState.Context.Empty).value
    println(ts)
  }
