package io.taig.otter.syntax

import io.taig.otter.operation.Enriched
import io.taig.otter.syntax.EnrichedSyntax.*
import io.taig.otter.Typescript
import io.taig.otter.TypescriptKeys

trait EnrichedTypescriptSyntax:
  extension [A](a: A)(using enriched: Enriched[A])
    def typescript(value: Typescript[Typescript.Value]): A = a.metadata(TypescriptKeys.typescript, value)
    def typescript(value: String): A = typescript(Typescript.Dynamic(value))

object EnrichedTypescriptSyntax extends EnrichedTypescriptSyntax
