package io.taig.otter

import cats.syntax.all.*
import cats.Show

final case class TypescriptZodDefinition(typescript: TypescriptDefinition, expression: String):
  override def toString: String =
    show"""$typescript
          |export const ${typescript.name}: z.ZodType<${typescript.name}> =
          |${indent(expression)}""".stripMargin

object TypescriptZodDefinition:
  given Show[TypescriptZodDefinition] = Show.fromToString