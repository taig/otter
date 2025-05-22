package io.taig.otter.codec

import io.taig.otter.component.JsonComponent.*
import io.taig.otter.Json
import io.taig.otter.Keys.*
import io.taig.otter.TypescriptState

object Playground:
  enum Animal:
    case Dog
    case Cat

  val animal: Json.Enumeration[Animal] = enumeration[Animal, String](string) {
    case Animal.Dog => "dog"
    case Animal.Cat => "cat"
  }.metadata(name, "Animal")

  val array: Json.Collection[?] = (collection.list(value)).metadata(name, "Arr")
  val primitive: Json.Union[?] = (boolean | int | string | constant(3) | animal).metadata(name, "Primitive")
  val value: Json.Union[?] = (array :+ primitive :+ dictionary.list(key.string, int)).metadata(name, "Value")

  @main
  def run = {
    val (context, ts) = JsonTypescriptRenderer.render(value).run(initial = TypescriptState.Context.Empty).value

    TypecriptReferencesPrinter
      .print(context.references)
      .foreach(println)
    println(ts)
  }
