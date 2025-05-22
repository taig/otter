package io.taig.otter

import io.taig.otter.component.JsonComponent.*
import io.taig.otter.codec.TypescriptZodPrinter
import io.taig.otter.codec.JsonTypescriptRenderer
import io.taig.otter.Keys.*
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

    val (refs, zod) = TypescriptZodPrinter.print(context.references, ts).run(ZodState.Context.Empty).value

    refs.references.foreach { case (name, zod) =>
      println(s"""type $name = ${zod.typescript}
                 |const $name: z.ZodType<${zod.typescript}> = ${zod.expression}""".stripMargin)
    }
  }
