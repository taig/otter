package io.taig.otter

import cats.Show
import cats.syntax.all.*

final case class TypescriptEffectEndpoint(
    input: TypescriptEffectDefinition,
    marker: String,
    types: List[TypescriptEffectDefinition],
    definition: String
)

object TypescriptEffectEndpoint:
  given Show[TypescriptEffectEndpoint] = endpoint => show"""${endpoint.marker}
                                                           |
                                                           |${endpoint.types.mkString_("\n\n")}
                                                           |
                                                           |${endpoint.definition}""".stripMargin
