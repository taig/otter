package io.taig.otter

import cats.Show
import cats.syntax.all.*
import io.taig.otter.codec.TypescriptPrinter
import io.taig.otter.codec.ZodPrinter

final case class TypescriptZodDefinition(name: String, value: TypescriptZod):
  override def toString: String =
    show"""export type $name = ${TypescriptPrinter.print(value.toTypescript)}
          |const $name: z.ZodType<$name> = ${ZodPrinter.print(value.toZod)}""".stripMargin

object TypescriptZodDefinition:
  given Show[TypescriptZodDefinition] = Show.fromToString
