package io.taig.otter

import io.taig.otter.component.JsonComponent.*
import io.taig.otter.codec.TypescriptZodPrinter
import io.taig.otter.codec.JsonTypescriptRenderer
import io.taig.otter.Keys.*
import io.taig.otter.codec.TypescriptZodPrinter2
import scala.collection.immutable.ListMap

object Playground:
  @main
  def run: Unit = {
    val (context, ts) =
      JsonTypescriptRenderer.render(schema = data.any).run(initial = TypescriptState.Context.Empty).value

    // context.references.foreach: (name, value) =>
    //   println(s"type $name = $value")

    // println(ts)

    // context.references.map((name, value) => s"const $name = ${TypescriptZodPrinter.print(value)}").foreach(println)
    // println(zod)

    val (refs, zod) = TypescriptZodPrinter2.print(context.references, ts).run(ListMap.empty).value

    refs.foreach { case (name, (ts, expression)) =>
      println(s"""type $name = $ts
                 |const $name: z.ZodType<$name> = $expression""".stripMargin)
    }
  }
