package io.taig.otter.syntax

import io.taig.otter.operation.Enriched
import io.taig.otter.syntax.EnrichedSyntax.*
import io.taig.otter.Typescript
import io.taig.otter.TypescriptKeys

trait TypescriptEnrichedSyntax:
  extension [A](a: A)(using enriched: Enriched[A])
    def typescript(value: Typescript): A = a.metadata(TypescriptKeys.typescript, value)

object TypescriptEnrichedSyntax extends TypescriptEnrichedSyntax
