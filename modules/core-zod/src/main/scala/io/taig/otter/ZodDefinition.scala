package io.taig.otter

import cats.Show
import cats.syntax.all.*

final case class ZodDefinition(name: String, value: Zod):
  override def toString: String = show"export const $name = $value"

object ZodDefinition:
  given Show[ZodDefinition] = Show.fromToString
