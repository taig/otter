package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.syntax.JsonSyntax.*

import scala.util.chaining.*
import scala.collection.immutable.ListMap
import io.taig.otter.Keys

// val JsonZodTypescriptRenderer: Renderer[Json, ListMap[String, String]] =
//   // JsonZodTypescriptExpressionRenderer.map: state =>
//   //   sta
//   ???

object Playground:
  import io.taig.otter.Json
  import io.taig.otter.component.JsonComponent.*
  import io.taig.otter.syntax.AnnotatedSyntax.*

  @main
  def run: Unit = {
    val a = string.attr(Keys.name, "A")

    val b = (string | int).attr(Keys.name, "B")

    val c = (field("foo", string.attr(Keys.name, "Yolo")) :* field("bar", a)).attr(Keys.name, "C")

    val jsons = List(a, b, c)

    val x = jsons.traverse(JsonZodTypescriptExpressionRenderer.render).runS(ListMap.empty).value

    println(x.map((key, value) => s"const $key = $value;").mkString("\n\n"))
  }
