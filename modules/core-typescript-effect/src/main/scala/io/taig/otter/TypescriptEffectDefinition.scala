package io.taig.otter

import cats.syntax.all.*
import cats.Show

final case class TypescriptEffectDefinition(name: String, value: TypescriptEffect)

object TypescriptEffectDefinition:
  given Show[TypescriptEffectDefinition] = definition =>
    val effect = definition.value.toEffect
    val tpe = definition.value.typescript
      .map(_.show)
      .getOrElse:
        show"typeof ${definition.name}.Type"

    show"""export type ${definition.name} = $tpe
          |export const ${definition.name} = $effect""".stripMargin
