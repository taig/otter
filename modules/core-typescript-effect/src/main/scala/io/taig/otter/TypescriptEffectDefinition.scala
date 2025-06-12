package io.taig.otter

import cats.syntax.all.*
import cats.Show

final case class TypescriptEffectDefinition(name: String, value: TypescriptEffect)

object TypescriptEffectDefinition:
  given Show[TypescriptEffectDefinition] = definition => show"${definition.name}"
